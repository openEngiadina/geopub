(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

module Evr = struct
  let instruct ?(propagate = true) ?(default = true) e =
    if default then () else Ev.prevent_default e;
    if propagate then () else Ev.stop_propagation e

  let on_el ?(capture = false) ?propagate ?default type' f el =
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
