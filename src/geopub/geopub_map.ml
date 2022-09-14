(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Lwt_react
open Brr
open Archi_lwt

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Map"

module Log = (val Logs.src_log src : Logs.LOG)

(* Handle map movement *)

type position = { latitude : float; longitude : float; zoom : int }

let init_position =
  { latitude = 46.794896096; longitude = 10.3003317118; zoom = 10 }

let position map =
  let position, set_position = S.create init_position in

  (* Set up a listener for map movement *)
  let () =
    let on_move_end _ =
      let latlng = Leaflet.Map.get_center map in
      let latitude, longitude = Leaflet.Latlng.(lat latlng, lng latlng) in
      let zoom = Leaflet.Map.get_zoom map in
      set_position { latitude; longitude; zoom }
    in
    Leaflet.Map.on Leaflet.Event.Move_end on_move_end map
  in

  (* set initial position and zoom *)
  Leaflet.Map.set_view
    (Leaflet.Latlng.create init_position.latitude init_position.longitude)
    ~zoom:(Some init_position.zoom) map;

  position

(* Query for visible RDF descriptions with geo location *)

module SubjectMap = Map.Make (Rdf.Triple.Subject)

let latlng_of_description description =
  let option_bind f opt = match opt with Some v -> f v | None -> None in

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
  in

  let wkt_point description =
    let geosparql =
      Rdf.Namespace.make_namespace "http://www.opengis.net/ont/geosparql#"
    in
    let float_parser =
      let open Angstrom in
      many_till any_char (char ' ' <|> char ')')
      >>| List.to_seq >>| String.of_seq
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
        @@ Rdf.Iri.of_string "http://www.opengis.net/ont/geosparql#hasGeometry"
        )
        description
    in
    match literal_opt with
    | Some literal ->
        if Rdf.Iri.equal (Rdf.Literal.datatype literal) (geosparql "wktLiteral")
        then parse (Rdf.Literal.canonical literal)
        else None
    | None -> None
  in

  match get_geo_latlng description with
  | Some latlng -> Some latlng
  | None -> (
      match wkt_point description with
      | Some latlng -> Some latlng
      | None -> None)

let visible_descriptions db position =
  let query =
    Database.Datalog.(
      Atom.make "triple-geo"
        Term.
          [
            make_variable "s";
            make_variable "p";
            make_variable "o";
            make_constant
            @@ Constant.GeoQuery (position.latitude, position.longitude, 4);
          ])
  in
  Database.query db query
  >|= S.map ~eq:Rdf.Graph.equal (fun (_tx, tuples) ->
          Database.Datalog.Tuple.Set.to_seq tuples
          |> Seq.filter_map (function
               | [ s; p; o; _ ] -> Some [ s; p; o ]
               | _ -> None)
          |> Seq.filter_map Database.triple_of_tuple
          |> Rdf.Graph.of_triples)
  >|= S.map ~eq:(SubjectMap.equal Rdf.Description.equal) (fun graph ->
          Rdf.Graph.descriptions graph
          |> Seq.map (fun description ->
                 (Rdf.Description.subject description, description))
          |> SubjectMap.of_seq)

(* Markers *)

let marker_of_description map description =
  match
    ( Rdf.Triple.Subject.to_iri @@ Rdf.Description.subject description,
      latlng_of_description description )
  with
  | Some iri, Some latlng ->
      let el = Ui_rdf.iri_plain iri in
      let marker = Leaflet.Layer.create_marker latlng in
      Leaflet.Layer.bind_popup el marker;
      Leaflet.Layer.add_to map marker;
      Some marker
  | _ -> None

(* Bind markers for all visible descriptions *)
let marker_updates map visible =
  E.fold
    (fun map_subjects visible_descriptions ->
      SubjectMap.merge
        (fun _subject marker_opt visible_description_opt ->
          match (marker_opt, visible_description_opt) with
          | Some marker, Some _ -> Some marker
          | Some marker, None ->
              Leaflet.Layer.remove marker;
              None
          | None, Some description -> marker_of_description map description
          | None, None -> None)
        map_subjects visible_descriptions)
    (* Initialize markers *)
    (S.value visible
    |> SubjectMap.filter_map (fun _subject description ->
           marker_of_description map description))
    (S.changes visible)
  |> E.map (fun _ -> ())

(* Map setup *)

(* A small hack to invalidate the size of the Leaflet map when it is
   dynamically loaded. If not it would not be displayed correctly until a
   manual window resize. *)
let setup_map_size_invalidator map =
  let body = Document.body G.document in

  let observer records _obs =
    let on_node node =
      match Jv.(to_option to_string @@ get node "id") with
      | Some "map" -> Leaflet.Map.invalidate_size map
      | _ -> ()
    in

    records
    |> Jv.to_list (fun x -> x)
    |> List.map (fun record ->
           Jv.to_list (fun x -> x) @@ Jv.get record "addedNodes")
    |> List.flatten |> List.iter on_node
  in
  let mutation_observer = Jv.get Jv.global "MutationObserver" in
  let observer = Jv.new' mutation_observer [| Jv.repr observer |] in
  let opts = Jv.obj [| ("childList", Jv.true'); ("subtree", Jv.false') |] in
  ignore @@ Jv.call observer "observe" [| El.to_jv body; opts |]

let init_leaflet router () =
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
              Router.set_route router @@ Route.Activity (Some latlng);
              Log.debug (fun m ->
                  m "Create post at %a/%a" Fmt.float
                    (Leaflet.Latlng.lat latlng)
                    Fmt.float
                    (Leaflet.Latlng.lng latlng)) );
      ]
  in

  (* create Leaflet map with context menu *)
  let leaflet =
    Leaflet.Map.create
      ~options:(Leaflet_contextmenu.options context_menu)
      map_container
  in

  (* add the OSM tile layer *)
  let tile_layer = Leaflet.Layer.create_tile_osm None in
  Leaflet.Layer.add_to leaflet tile_layer;

  (* Invalidate map size when it is added to the DOM *)
  setup_map_size_invalidator leaflet;

  leaflet

(* Component *)

type t = {
  leaflet : Leaflet.Map.t;
  position : position signal;
  marker_updates : unit event;
}

let start _ database router =
  (* initialize the Leaflet map *)
  let leaflet = init_leaflet router () in

  (* watch the position *)
  let position =
    position leaflet
    |> S.map (fun position ->
           Log.debug (fun m ->
               m "Map position: (%f, %f, %d)" position.latitude
                 position.longitude position.zoom);
           position)
  in

  (* query for visible things *)
  let* visible =
    S.bind_s
      ~eq:(SubjectMap.equal Rdf.Description.equal)
      position
      (visible_descriptions database)
    (* >|= S.map (fun descriptions ->
     *         Log.debug (fun m ->
     *             m "Count of visible descriptions: %d"
     *               (SubjectMap.cardinal descriptions));
     *         descriptions) *)
  in

  (* set markers for visible things *)
  let marker_updates = marker_updates leaflet visible in

  return_ok { leaflet; position; marker_updates }

let stop _ = return_unit

let component =
  Component.using ~start ~stop
    ~dependencies:[ Database.component; Router.component ]

(* View *)

let view t = Leaflet.Map.get_container t.leaflet
