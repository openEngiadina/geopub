(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(** GeoPub Window location Router

This component starts a router that exposes a signal that holds the
current route. *)

open Lwt_react
open Archi_lwt

type t = Route.t signal

val component : (string -> unit, t) Component.t
(** The router component *)

val current : t -> Route.t
val set_route : t -> Route.t -> unit
