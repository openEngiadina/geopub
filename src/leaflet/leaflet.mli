(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
(** {1 Leaflet}

This module provides bindings to the Leaflet JavaScript library for
mobile-friendly interactive maps.

See also the [Leaflet API reference](https://leafletjs.com/reference.html).
*)

module LatLng : sig
  type t

  val create : float -> float -> t
  val lat : t -> float
  val lng : t -> float
end

module Ev : sig
  module MouseEvent : sig
    type t

    val latlng : t -> LatLng.t
  end
end

module Map : sig
  type t

  val create : ?options:Jv.t -> El.t -> t
  val invalidate_size : t -> unit
  val set_view : LatLng.t -> zoom:int -> t -> t
  val fit_world : t -> unit
  val get_container : t -> El.t

  (** {1 Events} **)

  val as_target : t -> Brr.Ev.target

  (** {2 Interaction events} **)

  val click : Ev.MouseEvent.t Brr.Ev.type'
end

module TileLayer : sig
  type t

  val create_osm : unit -> t
  val add_to : t -> Map.t -> unit
end

module Marker : sig
  type t

  val create : LatLng.t -> t
  val add_to : t -> Map.t -> unit

  (** {2 Popup methods} *)

  val bind_popup : El.t -> t -> t
  val open_popup : t -> unit
end
