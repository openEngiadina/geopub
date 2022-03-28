(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt

(* Setup logging *)
let src = Logs.Src.create "Geopub_map"

module Log = (val Logs.src_log src : Logs.LOG)

type t = Leaflet.Map.t

(* We need a Init message to pass the `send_msg` function to the
   Leaflet map. The Reactor init function does not pass this, only the
   update. So the map needs to be initialized with a message. Another
   thing that seems wrong with Reactor... *)
(* type msg = Init | SetView of Leaflet.LatLng.t *)

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
          m "Create post at %a/%a" Fmt.float
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

let view model = return @@ Leaflet.Map.get_container model
