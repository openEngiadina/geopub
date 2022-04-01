(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax
open Lwt_react

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
  | Some lat, Some long -> Some (Leaflet.LatLng.create lat long)
  | _ -> None

let geo_objects =
  S.accum_s
    (E.map
       (fun db _ ->
         Database.get_with_geo db
         >|= List.filter_map (fun description ->
                 match get_latlng description with
                 | Some latlng -> Some (latlng, description)
                 | None -> None))
       Database.Triples.on_update)
    []

(* mutable map of added descriptions *)

let added_geo_objects = ref SubjectSet.empty

let add_geo_object db map (latlng, description) =
  if SubjectSet.mem (Rdf.Description.subject description) !added_geo_objects
  then return_unit
  else
    let* el = Ui_rdf.view_subject db @@ Rdf.Description.subject description in
    let marker = Leaflet.Marker.create latlng |> Leaflet.Marker.bind_popup el in
    return @@ Leaflet.Marker.add_to marker map

let init () =
  let map_container = El.div ~at:At.[ id @@ Jstr.v "map" ] [] in

  (* create a context menu *)
  let context_menu =
    Leaflet_contextmenu.Menu.
      [
        Callback
          ( "Create post here",
            fun e ->
              let latlng = Leaflet.Ev.MouseEvent.latlng e in
              Log.debug (fun m ->
                  m "Create post at %a/%a" Fmt.float
                    (Leaflet.LatLng.lat latlng)
                    Fmt.float
                    (Leaflet.LatLng.lng latlng)) );
      ]
  in

  (* create map with context menu *)
  let map =
    Leaflet.Map.create
      ~options:(Leaflet_contextmenu.options context_menu)
      map_container
  in

  (* Set up a listener for clicks on the map (currently not used) *)
  Ev.listen Leaflet.Map.click (fun e ->
      let latlng = e |> Ev.as_type |> Leaflet.Ev.MouseEvent.latlng in
      Log.debug (fun m ->
          m "Click at %a/%a" Fmt.float
            (Leaflet.LatLng.lat latlng)
            Fmt.float
            (Leaflet.LatLng.lng latlng)))
  @@ Leaflet.Map.as_target map;

  (* add the OSM tile layer *)
  let tile_layer = Leaflet.TileLayer.create_osm () in
  Leaflet.TileLayer.add_to tile_layer map;

  (* return Map *)
  map
  |> Leaflet.(Map.set_view LatLng.(create 46.794896096 10.3003317118) ~zoom:10)
  |> return

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

let view db map =
  let* () = Lwt_list.iter_s (add_geo_object db map) (S.value geo_objects) in

  return @@ Leaflet.Map.get_container map
