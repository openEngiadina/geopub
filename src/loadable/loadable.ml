(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

type ('a, 'e) t = Idle | Loading | Loaded of 'a | Error of 'e

let map f = function
  | Idle -> Idle
  | Loading -> Loading
  | Loaded v -> Loaded (f v)
  | Error e -> Error e

let to_option = function
  | Idle -> None
  | Loading -> None
  | Loaded v -> Some v
  | Error _ -> None

let of_result = function Ok v -> Loaded v | Result.Error e -> Error e
