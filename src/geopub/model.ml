(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Geopub_database

type t = { database : Database.t; route : Route.t; map : Geopub_map.t }
