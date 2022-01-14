(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

type t =
  | About
  | Login
  | Map
  | Activity
  | Roster
  | RosterItem of Xmpp.Jid.t
  | AddContact

type action = NewPost of Leaflet.LatLng.t option
