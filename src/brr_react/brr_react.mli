(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open React

module Evf : sig
  val on_el :
    ?capture:bool ->
    ?propagate:bool ->
    ?default:bool ->
    'b Ev.type' ->
    ('b Ev.t -> unit) ->
    El.t ->
    El.t
end

module Elr : sig
  val def_children : El.t -> El.t list signal -> unit signal
end
