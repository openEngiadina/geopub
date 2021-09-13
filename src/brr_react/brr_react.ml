(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* Parts are taken from brr_note.el which is part of the Brr package
 * provided under the ISC license. *)

open Brr
open React

module Evr = struct
  let instruct ?(propagate = true) ?(default = true) e =
    if default then () else Ev.prevent_default e;
    if propagate then () else Ev.stop_propagation e

  let on_target ?(capture = false) ?propagate ?default type' f t =
    let opts =
      match capture with
      | false -> None
      | true -> Some (Ev.listen_opts ~capture ())
    in
    let e, send_e = E.create () in
    let f ev =
      instruct ?propagate ?default ev;
      send_e (f ev)
    in
    Ev.listen ?opts type' f t;
    e

  let on_el ?capture ?propagate ?default type' f el =
    on_target ?capture ?propagate ?default type' f (El.as_target el)
end

module Elr = struct
  let def_children e cs = cs |> S.map (El.set_children e)
end
