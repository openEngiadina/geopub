(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

type t = {
  router : Router.t;
  database : Database.t;
  user : User.t;
  map : Geopub_map.t;
}
