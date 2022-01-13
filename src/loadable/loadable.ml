(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

type 'a t = Idle | Loading | Loaded of 'a

let map f = function
  | Idle -> Idle
  | Loading -> Loading
  | Loaded v -> Loaded (f v)

let to_option = function Idle -> None | Loading -> None | Loaded v -> Some v
