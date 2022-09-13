(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open React

module Elr = struct
  let def_children e cs = cs |> S.map (El.set_children e)
end
