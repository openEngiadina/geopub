(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

module Database = Geopub_database

type t = {
  router : Router.t;
  database : Database.t;
  xmpp_connection : Xmpp.Connection.t;
}
