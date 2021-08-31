(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)
open Lwt
open Brr
open Brr_react
open Lwt_react
open Js_of_ocaml_lwt

let body, set_body = S.create []

let _body_update_s = Elr.def_children (Document.body G.document) body

let main = Lwt_js.sleep 1.0 >|= fun () -> set_body El.[ txt' "Hello world!" ]

let () = ignore main
