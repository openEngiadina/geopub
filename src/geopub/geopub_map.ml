(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt_react
open Lwt.Syntax

(* Setup logging *)
let src = Logs.Src.create "Geopub_map"

module Log = (val Logs.src_log src : Logs.LOG)

type t = Leaflet.Map.t

(* Extract Latitude and Longitude from RDF *)

(* Option.bind, when? *)
let option_bind f opt = match opt with Some v -> f v | None -> None

let get_geo_latlng description =
  let lat =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.geo "lat")
      description
    |> option_bind Rdf.Triple.Object.to_literal
    |> Option.map Rdf.Literal.canonical
    |> option_bind float_of_string_opt
  in
  let long =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.geo "long")
      description
    |> option_bind Rdf.Triple.Object.to_literal
    |> Option.map Rdf.Literal.canonical
    |> option_bind float_of_string_opt
  in

  match (lat, long) with
  | Some lat, Some long -> Some (Leaflet.Latlng.create lat long)
  | _ -> None

let wkt_point description =
  let geosparql =
    Rdf.Namespace.make_namespace "http://www.opengis.net/ont/geosparql#"
  in
  let float_parser =
    let open Angstrom in
    many_till any_char (char ' ' <|> char ')') >>| List.to_seq >>| String.of_seq
    >>= fun s ->
    match float_of_string_opt s with
    | Some f -> return f
    | None -> fail "could not parse float in wkt POINT"
  in
  let parser =
    let open Angstrom in
    (* Note lat and long seem to be switched up in WKT points *)
    (fun _ long lat -> Leaflet.Latlng.create lat long)
    <$> string "POINT(" <*> float_parser <*> float_parser
  in
  let parse s =
    Angstrom.parse_string ~consume:Angstrom.Consume.All parser s
    |> Result.to_option
  in
  let literal_opt =
    Rdf.Description.functional_property_literal
      (Rdf.Triple.Predicate.of_iri
      @@ Rdf.Iri.of_string "http://www.opengis.net/ont/geosparql#hasGeometry")
      description
  in
  match literal_opt with
  | Some literal ->
      if Rdf.Iri.equal (Rdf.Literal.datatype literal) (geosparql "wktLiteral")
      then parse (Rdf.Literal.canonical literal)
      else None
  | None -> None

let get_latlng description =
  match get_geo_latlng description with
  | Some latlng -> Some latlng
  | None -> (
      match wkt_point description with
      | Some latlng -> Some latlng
      | None -> None)

(* State management *)

module SubjectMap = Map.Make (Rdf.Triple.Subject)
module SubjectSet = Set.Make (Rdf.Triple.Subject)

let query_visible db (lat, long, _zoom) =
  let query =
    Database.Datalog.(
      Atom.make "triple-geo"
        Term.
          [
            make_variable "s";
            make_variable "p";
            make_variable "o";
            make_constant @@ Constant.GeoQuery (lat, long, 4);
          ])
  in
  let* graph =
    Database.query db query |> Lwt_seq.return_lwt
    |> Lwt_seq.flat_map (fun set ->
           Lwt_seq.of_seq @@ Database.Datalog.Tuple.Set.to_seq set)
    |> Lwt_seq.filter_map_s (function
         | Database.Datalog.Constant.[ Rdf s_id; Rdf p_id; Rdf o_id; _ ] ->
             Database.Store.Triples.deref db [ s_id; p_id; o_id ]
         | _ -> return_none)
    |> Lwt_seq.fold_left
         (fun graph triple -> Rdf.Graph.add triple graph)
         Rdf.Graph.empty
  in
  Rdf.Graph.descriptions graph
  |> Seq.map (fun description ->
         (Rdf.Description.subject description, description))
  |> SubjectMap.of_seq |> return

let init db ~set_route () =
  (* create and append to body map_container *)
  let map_container = Brr.El.div ~at:Brr.At.[ id @@ Jstr.v "map" ] [] in
  Brr.El.append_children
    (Brr.Document.body Brr.G.document)
    [ Brr.El.div ~at:Brr.At.[ hidden ] [ map_container ] ];

  (* create a context menu *)
  let context_menu =
    Leaflet_contextmenu.Menu.
      [
        Callback
          ( "Create post here",
            fun e ->
              let latlng = Leaflet.Event.latlng e in
              set_route @@ Route.Activity (Some latlng);
              Log.debug (fun m ->
                  m "Create post at %a/%a" Fmt.float
                    (Leaflet.Latlng.lat latlng)
                    Fmt.float
                    (Leaflet.Latlng.lng latlng)) );
      ]
  in

  (* create map with context menu *)
  let map =
    Leaflet.Map.create
      ~options:(Leaflet_contextmenu.options context_menu)
      map_container
  in

  (* Position and Zoom *)
  let pos_zoom, set_pos_zoom = S.create (46.794896096, 10.3003317118, 10) in

  (* Set up a listener for clicks on the map (currently not used) *)
  let () =
    let on_move_end _ =
      let latlng = Leaflet.Map.get_center map in
      let lat, long = Leaflet.Latlng.(lat latlng, lng latlng) in
      let zoom = Leaflet.Map.get_zoom map in
      set_pos_zoom (lat, long, zoom)
    in
    Leaflet.Map.on Leaflet.Event.Move_end on_move_end map
  in

  (* add the OSM tile layer *)
  let tile_layer = Leaflet.Layer.create_tile_osm None in
  Leaflet.Layer.add_to map tile_layer;

  (* set initial position and zoom *)
  Leaflet.Map.set_view
    (Leaflet.Latlng.create 46.794896096 10.3003317118)
    ~zoom:(Some 10) map;

  set_pos_zoom (46.794896096, 10.3003317118, 10);

  (* Manage visible objects *)
  let visible_descriptions =
    S.changes pos_zoom |> E.map_p (fun pos_zoom -> query_visible db pos_zoom)
  in

  E.fold
    (fun map_subjects visible_descriptions ->
      SubjectMap.merge
        (fun subject marker_opt visible_description_opt ->
          match (marker_opt, visible_description_opt) with
          | Some marker, Some _ -> Some marker
          | Some marker, None ->
              Leaflet.Layer.remove marker;
              None
          | None, Some description -> (
              match
                (Rdf.Triple.Subject.to_iri subject, get_latlng description)
              with
              | Some iri, Some latlng ->
                  let el = Ui_rdf.view_iri iri in
                  let marker = Leaflet.Layer.create_marker latlng in
                  Leaflet.Layer.bind_popup el marker;
                  Leaflet.Layer.add_to map marker;
                  Some marker
              | _ -> None)
          | None, None -> None)
        map_subjects visible_descriptions)
    SubjectMap.empty visible_descriptions
  |> E.keep;

  S.map
    (fun (lat, long, zoom) ->
      Log.debug (fun m -> m "Map position and zoom: (%f, %f, %d)" lat long zoom))
    pos_zoom
  |> S.keep;

  (* return Map *)
  map |> return

(* functions to modify map *)

let invalidate_size model = Leaflet.Map.invalidate_size model
let view map = return @@ Leaflet.Map.get_container map
