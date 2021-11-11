(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Reactor
open Brr
open Lwt

type t = Leaflet.Map.t option

(* We need a Init message to pass the `send_msg` function to the
   Leaflet map. The Reactor init function does not pass this, only the
   update. So the map needs to be initialized with a message. Another
   thing that seems wrong with Reactor... *)
type msg = Init | SetView of Leaflet.LatLng.t

let create send_msg =
  let map_container = El.div ~at:At.[ id (Jstr.v "map") ] [] in
  let context_menu =
    Leaflet_contextmenu.Menu.
      [
        Callback
          ( "Create post here",
            fun e ->
              let latlng = Leaflet.Ev.MouseEvent.latlng e in
              send_msg @@ `SetRoute (Route.Posts (Some latlng)) );
      ]
  in
  let map =
    Leaflet.Map.create
      ~options:(Leaflet_contextmenu.options context_menu)
      map_container
  in

  Ev.listen Leaflet.Map.click (fun e ->
      Console.log [ e |> Ev.as_type |> Leaflet.Ev.MouseEvent.latlng ])
  @@ Leaflet.Map.as_target map;
  let tile_layer = Leaflet.TileLayer.create_osm () in
  Leaflet.TileLayer.add_to tile_layer map;
  map
  |> Leaflet.(Map.set_view LatLng.(create 46.794896096 10.3003317118) ~zoom:10)

let update ~send_msg model msg =
  ignore send_msg;
  ignore model;
  match msg with
  | Init -> create send_msg |> Option.some |> Return.singleton
  | SetView latlng ->
      Option.map (fun map -> map |> Leaflet.Map.set_view latlng ~zoom:10) model
      |> Return.singleton

let view _send_msg map =
  match map with
  | Some map -> return [ Leaflet.Map.get_container map ]
  | _ -> return_nil
