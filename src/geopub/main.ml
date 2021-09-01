(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)
open Lwt
open Brr
open Brr_react
open Lwt_react
(* open Js_of_ocaml_lwt *)

type model = El.t

let model_s, update_model = S.create @@ El.div ~at:At.[ id (Jstr.v "map") ] []

let topbar =
  El.(
    div
      ~at:At.[ id (Jstr.v "topbar") ]
      [
        header [ img ~at:At.[ src (Jstr.v "sgraffito.svg") ] () ];
        nav [ ul [ li [ txt' "Hi" ] ] ];
        nav [ ul [ li [ txt' "About" ] ] ];
      ])

let view model = El.[ topbar; div ~at:At.[ id (Jstr.v "main") ] [ model ] ]

let main =
  let _view_s =
    Elr.def_children (Document.body G.document) S.(map view model_s)
  in
  let map_container = S.value model_s in
  let map = Leaflet.create map_container in
  Leaflet.(set_view map LatLng.(create 63.4275 10.4109) 11);
  let tile_layer = Leaflet.TileLayer.create_osm () in
  Leaflet.TileLayer.add_to map tile_layer;
  return_unit

let () = ignore main
