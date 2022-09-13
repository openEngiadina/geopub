(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open React

module Evf = struct
  let on_el ?(capture = false) ?propagate ?default type' f el =
    let instruct ?(propagate = true) ?(default = true) e =
      if default then () else Ev.prevent_default e;
      if propagate then () else Ev.stop_propagation e
    in
    let opts =
      match capture with
      | false -> None
      | true -> Some (Ev.listen_opts ~capture ())
    in
    let f ev =
      instruct ?propagate ?default ev;
      f ev
    in
    Ev.listen ?opts type' f (El.as_target el);
    el
end

module Elr = struct
  let def_children e cs = cs |> S.map (El.set_children e)
end
