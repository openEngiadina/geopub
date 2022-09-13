(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

(* Helper to create classes *)
let class' name = At.class' @@ Jstr.v name

(* UIKit stuff *)

let container = class' "uk-container"

module Container = struct
  let expand = class' "uk-container-expand"
end

module Align = struct
  let left = class' "uk-align-left"
  let right = class' "uk-align-right"
  let center = class' "uk-align-center"
end

let margin = class' "uk-margin"

module Margin = struct
  let top = class' "uk-margin-top"
  let bottom = class' "uk-margin-bootm"
  let left = class' "uk-margin-left"
  let right = class' "uk-margin-right"
end

module Position = struct
  let center = class' "uk-position-center"
end

let button = class' "uk-button"

module Button = struct
  let default = class' "uk-button-default"
  let primary = class' "uk-button-primary"
  let link = class' "uk-button-link"
end

module Form = struct
  let stacked = class' "uk-form-stacked"
  let input = class' "uk-input"
  let label = class' "uk-form-label"
  let controls = class' "uk-form-controls"
end

let spinner = At.true' @@ Jstr.v "uk-spinner"
let close = At.true' @@ Jstr.v "uk-close"
let alert = At.true' @@ Jstr.v "uk-alert"

module Alert = struct
  let close = class' "uk-alert-close"
  let danger = class' "uk-alert-danger"
end

let article = class' "uk-article"

module Article = struct
  let title = class' "uk-article-title"
  let meta = class' "uk-article-meta"
end

let description_list = class' "uk-description-list"
