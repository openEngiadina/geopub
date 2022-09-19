(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

(* Helper to create classes *)
let class' name = At.class' @@ Jstr.v name

(* UIKit stuff *)

let uikit = Jv.(get global "UIkit")

(* Components *)

let container = class' "uk-container"

module Container = struct
  let expand = class' "uk-container-expand"
end

let open' = class' "uk-open"
let grid = At.true' @@ Jstr.v "uk-grid"
let accordion = At.true' @@ Jstr.v "uk-accordion"

module Accordion = struct
  let multiple = At.v (Jstr.v "uk-accordion") (Jstr.v "multiple: true")
  let title = class' "uk-accordion-title"
  let content = class' "uk-accordion-content"
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

let padding = class' "uk-padding"

module Position = struct
  let center = class' "uk-position-center"
end

let button = class' "uk-button"

module Button = struct
  let default = class' "uk-button-default"
  let primary = class' "uk-button-primary"
  let secondary = class' "uk-button-secondary"
  let link = class' "uk-button-link"
end

let section = class' "uk-section"

module Section = struct
  let default = class' "uk-section-default"
  let muted = class' "uk-section-muted"
  let primary = class' "uk-section-primary"
  let secondary = class' "uk-section-secondary"
end

module Form = struct
  let stacked = class' "uk-form-stacked"
  let input = class' "uk-input"
  let textarea = class' "uk-textarea"
  let label = class' "uk-form-label"
  let controls = class' "uk-form-controls"
end

let spinner = At.true' @@ Jstr.v "uk-spinner"
let close = At.true' @@ Jstr.v "uk-close"
let alert = At.true' @@ Jstr.v "uk-alert"

module Alert = struct
  let close = class' "uk-alert-close"
  let danger = class' "uk-alert-danger"
  let warning = class' "uk-alert-warning"
end

let article = class' "uk-article"

module Article = struct
  let title = class' "uk-article-title"
  let meta = class' "uk-article-meta"
end

let list = class' "uk-list"

module List = struct
  let large = class' "uk-list-large"
end

let description_list = class' "uk-description-list"
let comment = class' "uk-comment"

module Comment = struct
  let header = class' "uk-comment-header"
  let meta = class' "uk-comment-meta"
  let body = class' "uk-comment-body"
  let title = class' "uk-comment-title"
  let list = class' "uk-comment-list"
  let primary = class' "uk-comment-primary"
end

let width n d =
  At.class' @@ Jstr.v @@ "uk-width-" ^ string_of_int n ^ "-" ^ string_of_int d

module Width = struct
  let expand = class' "uk-width-expand"
end

module Height = struct
  let small = class' "uk-height-small"
  let medium = class' "uk-height-medium"
  let large = class' "uk-height-large"
end

let subnav = class' "uk-subnav"
let active = class' "uk-active"

module Subnav = struct
  let divider = class' "uk-subnav-divider"
end

let iconnav = class' "uk-iconnav"

module Iconnav = struct
  let vertical = class' "uk-iconnav-vertical"
end

module Icon = struct
  let v name = At.v (Jstr.v "uk-icon") (Jstr.v name)
  let code = v "code"
  let star = v "star"
  let plus = v "plus"
  let mail = v "mail"
  let comment = v "comment"
  let forward = v "forward"
end

module Text = struct
  let truncate = class' "uk-text-truncate"
  let break = class' "uk-text-break"
end

let offcanvas = At.true' @@ Jstr.v "uk-offcanvas"

module Offcanvas = struct
  let bar = class' "uk-offcanvas-bar"
  let close = class' "uk-offcanvas-close"

  let show el =
    ignore @@ Jv.(call (call uikit "offcanvas" [| El.to_jv el |]) "show" [||])

  let hide el =
    ignore @@ Jv.(call (call uikit "offcanvas" [| El.to_jv el |]) "hide" [||])
end
