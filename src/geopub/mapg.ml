(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt

type t = Leaflet.Map.t

(* This is a hack to prevent the listener to be setup multiple times
   (everytime the map view is loaded). This is a clear indication that
   something is wrong with Reactor...*)
let listener_setup = ref false

let create () =
  let map_container = El.div ~at:At.[ id (Jstr.v "map") ] [] in
  let map = Leaflet.Map.create map_container in
  let tile_layer = Leaflet.TileLayer.create_osm () in
  Leaflet.TileLayer.add_to tile_layer map;
  Leaflet.(Map.set_view map LatLng.(create 63.4275 10.4109) 11);
  map

let view _send_msg map =
  if not !listener_setup then (
    listener_setup := true;
    Ev.listen Leaflet.Map.click (fun e ->
        Console.log [ e |> Ev.as_type |> Leaflet.Ev.MouseEvent.latlng ])
    @@ Leaflet.Map.as_target map)
  else ();

  return [ Leaflet.Map.get_container map ]
