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

module LatLng = struct
  type t = Jv.t

  let create lat lng =
    Jv.call leaflet "latLng" [| Jv.of_float lat; Jv.of_float lng |]

  let lat latlng = Jv.get latlng "lat" |> Jv.to_float

  let lng latlng = Jv.get latlng "lng" |> Jv.to_float
end

module Ev = struct
  module MouseEvent = struct
    type t = Jv.t

    let latlng e = Jv.get e "latlng"
  end
end

module Map = struct
  type t = Jv.t

  let create ?(options = Jv.null) el =
    Jv.call leaflet "map" [| El.to_jv el; options |]

  let invalidate_size map =
    ignore @@ Jv.call map "invalidateSize" [| Jv.true' |]

  let fit_world map = ignore @@ Jv.call map "fitWorld" [||]

  let get_container map = Jv.call map "getContainer" [||] |> El.of_jv

  let set_view latlng ~zoom map =
    ignore @@ Jv.call map "setView" [| latlng; Jv.of_int zoom |];
    map

  let as_target map = Brr.Ev.target_of_jv map

  let click = Brr.Ev.Type.create (Jstr.v "click")
end

module TileLayer = struct
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

  let add_to tile_layer map = ignore @@ Jv.call tile_layer "addTo" [| map |]
end

module Marker = struct
  type t = Jv.t

  let create latlng = Jv.call leaflet "marker" [| latlng |]

  let add_to marker map = ignore @@ Jv.call marker "addTo" [| map |]

  let bind_popup el marker =
    ignore @@ Jv.call marker "bindPopup" [| El.to_jv el |];
    marker

  let open_popup marker = ignore @@ Jv.call marker "openPopup" [||]
end
