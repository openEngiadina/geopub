(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax

(* Setup logging *)
let src = Logs.Src.create "Geopub_map"

module Log = (val Logs.src_log src : Logs.LOG)
module Database = Geopub_database

type t = Leaflet.Map.t

module SubjectSet = Set.Make (struct
  type t = Rdf.Triple.Subject.t

  let compare = Rdf.Triple.Subject.compare
end)

(* Option.bind, when? *)
let option_bind f opt = match opt with Some v -> f v | None -> None

let get_latlng description =
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

let init ~set_route () =
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

  (* Set up a listener for clicks on the map (currently not used) *)
  let () =
    let on_click e =
      let latlng = Leaflet.Event.latlng e in
      Log.debug (fun m ->
          m "Click at %a/%a" Fmt.float
            (Leaflet.Latlng.lat latlng)
            Fmt.float
            (Leaflet.Latlng.lng latlng))
    in
    Leaflet.Map.on Leaflet.Event.Click on_click map
  in

  (* add the OSM tile layer *)
  let tile_layer = Leaflet.Layer.create_tile_osm None in
  Leaflet.Layer.add_to map tile_layer;

  (* set view *)
  Leaflet.Map.set_view
    (Leaflet.Latlng.create 46.794896096 10.3003317118)
    ~zoom:(Some 10) map;

  (* return Map *)
  map |> return

(* functions to modify map *)

let invalidate_size model = Leaflet.Map.invalidate_size model

(* let add_geoloc geoloc model =
 *   let marker = Geoloc.to_latlng geoloc |> Leaflet.Marker.create in
 *   Leaflet.Marker.add_to marker model.leaflet;
 *   model
 * 
 * let add_rdf rdf model =
 *   Activitystreams.activities rdf
 *   |> Seq.filter_map (Activitystreams.get_object rdf)
 *   |> Seq.filter_map (Activitystreams.get_geoloc rdf)
 *   |> Seq.fold_left (fun model geoloc -> add_geoloc geoloc model) model
 * 
 * let add_post post model =
 *   match post |> Xep_0277.Post.to_marker with
 *   | Some marker ->
 *       Leaflet.Marker.add_to marker model.leaflet;
 *       model
 *   | None -> model
 * 
 * let set_view latlng model =
 *   { model with leaflet = Leaflet.Map.set_view latlng ~zoom:10 model.leaflet }
 * 
 * (\* Subscription and view *\)
 * 
 * let subscriptions model = model.events |> Lwt_react.E.of_stream *)

(* mutable map of added descriptions *)

let added_geo_objects = ref SubjectSet.empty

let add_geo_object db map (latlng, description) =
  if SubjectSet.mem (Rdf.Description.subject description) !added_geo_objects
  then return_unit
  else
    let* el = Ui_rdf.view_subject db @@ Rdf.Description.subject description in
    let marker = Leaflet.Layer.create_marker latlng in
    Leaflet.Layer.bind_popup el marker;
    return @@ Leaflet.Layer.add_to map marker

let view db map =
  let* () =
    Database.get_with_geo db |> Lwt_seq.to_list
    >|= List.filter_map (fun description ->
            match get_latlng description with
            | Some latlng -> Some (latlng, description)
            | None -> (
                match wkt_point description with
                | Some latlng -> Some (latlng, description)
                | None -> None))
    >>= Lwt_list.iter_s (add_geo_object db map)
  in
  return @@ Leaflet.Map.get_container map
