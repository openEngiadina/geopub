(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

type t = Leaflet.Map.t

let create () =
  let map_container = El.div ~at:At.[ id (Jstr.v "map") ] [] in
  let map = Leaflet.Map.create map_container in
  let tile_layer = Leaflet.TileLayer.create_osm () in
  Leaflet.TileLayer.add_to map tile_layer;
  Leaflet.(Map.set_view map LatLng.(create 63.4275 10.4109) 11);
  map
