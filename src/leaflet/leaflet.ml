(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)
open Brr

let leaflet =
  match Jv.(find global "L") with
  | Some l -> l
  | None -> failwith "Could not load Leaflet"

type t = Jv.t

let create el = Jv.call leaflet "map" [| El.to_jv el |]

let invalidate_size map = ignore @@ Jv.call map "invalidateSize" [| Jv.true' |]

let fit_world map = ignore @@ Jv.call map "fitWorld" [||]

let get_container map = Jv.call map "getContainer" [||] |> El.of_jv

module LatLng = struct
  type t = Jv.t

  let create lat lng =
    Jv.call leaflet "latLng" [| Jv.of_float lat; Jv.of_float lng |]
end

let set_view map latlng zoom =
  ignore @@ Jv.call map "setView" [| latlng; Jv.of_int zoom |]

module TileLayer = struct
  type map = t

  type t = Jv.t

  let create_osm () =
    Jv.call leaflet "tileLayer"
      [|
        Jv.of_string "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png";
        Jv.obj
          [|
            ( "attribution",
              Jv.of_string
                "&copy; <a \
                 href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> \
                 contributors" );
          |];
      |]

  let add_to map tile_layer = ignore @@ Jv.call tile_layer "addTo" [| map |]
end
