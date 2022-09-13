(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

let container = At.class' @@ Jstr.v "uk-container"

module Container = struct
  let expand = At.class' @@ Jstr.v "uk-container-expand"
end

module Align = struct
  let left = At.class' @@ Jstr.v "uk-align-left"
  let right = At.class' @@ Jstr.v "uk-align-right"
  let center = At.class' @@ Jstr.v "uk-align-center"
end

let margin = At.class' @@ Jstr.v "uk-margin"

module Margin = struct
  let top = At.class' @@ Jstr.v "uk-margin-top"
  let bottom = At.class' @@ Jstr.v "uk-margin-bootm"
  let left = At.class' @@ Jstr.v "uk-margin-left"
  let right = At.class' @@ Jstr.v "uk-margin-right"
end

module Position = struct
  let center = At.class' @@ Jstr.v "uk-position-center"
end
