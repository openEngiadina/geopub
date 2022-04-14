(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

module Database = Geopub_database
module Xmpp = Geopub_xmpp

type t = {
  database : Database.t;
  activities : Rdf.Description.t list;
  route : Route.t;
  map : Geopub_map.t;
  xmpp : (Xmpp.t, exn) Loadable.t;
}
