(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

type t =
  | About
  | Login
  | Map
  | Posts of Leaflet.LatLng.t option
  | Roster of Xmppl.Jid.t
  | AddContact

type action = [ `SetRoute of t ]
