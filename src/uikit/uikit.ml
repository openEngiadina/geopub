(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

module Margin = struct
  let top = At.class' @@ Jstr.v "uk-margin-top"
  let bottom = At.class' @@ Jstr.v "uk-margin-bootm"
  let left = At.class' @@ Jstr.v "uk-margin-left"
  let right = At.class' @@ Jstr.v "uk-margin-right"
end

let margin = At.class' @@ Jstr.v "uk-margin"
