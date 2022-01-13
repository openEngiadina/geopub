(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt_react

type msg = CreatePost of Leaflet.LatLng.t
type t = { events : msg E.t; leaflet : Leaflet.Map.t }

(* We need a Init message to pass the `send_msg` function to the
   Leaflet map. The Reactor init function does not pass this, only the
   update. So the map needs to be initialized with a message. Another
   thing that seems wrong with Reactor... *)
(* type msg = Init | SetView of Leaflet.LatLng.t *)

let init () =
  let events, send_msg = E.create () in
  let map_container = El.div ~at:At.[ id @@ Jstr.v "map" ] [] in

  (* create a context menu *)
  let context_menu =
    Leaflet_contextmenu.Menu.
      [
        Callback
          ( "Create post here",
            fun e ->
              let latlng = Leaflet.Ev.MouseEvent.latlng e in
              send_msg @@ CreatePost latlng );
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
      Console.log [ e |> Ev.as_type |> Leaflet.Ev.MouseEvent.latlng ])
  @@ Leaflet.Map.as_target map;

  (* add the OSM tile layer *)
  let tile_layer = Leaflet.TileLayer.create_osm () in
  Leaflet.TileLayer.add_to tile_layer map;

  (* return map state *)
  {
    events;
    leaflet =
      (* set map view to default location *)
      (map
      |> Leaflet.(
           Map.set_view LatLng.(create 46.794896096 10.3003317118) ~zoom:10));
  }

(* functions to modify map *)

let invalidate_size model = Leaflet.Map.invalidate_size model.leaflet

let add_post post model =
  match post |> Xep_0277.Post.to_marker with
  | Some marker ->
      Leaflet.Marker.add_to marker model.leaflet;
      model
  | None -> model

(* Subscription and view *)

let subscriptions model = model.events

let view ~send_msg model =
  ignore send_msg;
  Leaflet.Map.get_container model.leaflet
