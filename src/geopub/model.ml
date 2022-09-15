(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

type t = {
  router : Router.t;
  xmpp : Xmpp.t;
  xmpp_rdf : Xmpp_rdf.t;
  database : Database.t;
  user : User.t;
  map : Geopub_map.t;
}
