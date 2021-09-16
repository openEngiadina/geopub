(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Lwt_react
open Reactor
open Reactor_brr
open Brr
open Brr_io
module Client = Xmpp_websocket.Client

type msg = Login of Xmpp.Jid.t * string | Authenticated of Client.t

type t = Init | Loading | Client of Client.t

let init () = Return.singleton Init

let login jid password =
  let* client =
    Client.create
      { ws_endpoint = Some "ws://localhost:5280/xmpp-websocket" }
      jid ~password
  in
  let* () = Client.connect client in
  return @@ Authenticated client

let update ~send_msg model = function
  | Login (jid, password) ->
      Loading |> Return.singleton |> Return.command (login jid password)
  | Authenticated client -> Client client |> Return.singleton

(* View *)

let login_view send_msg =
  let login_form =
    El.(
      form
        [
          (* JID *)
          label ~at:At.[ for' @@ Jstr.v "jid" ] [ txt' "JID" ];
          input
            ~at:
              At.
                [
                  id @@ Jstr.v "jid";
                  name @@ Jstr.v "jid";
                  type' @@ Jstr.v "text";
                ]
            ();
          (* Password *)
          label ~at:At.[ for' @@ Jstr.v "password" ] [ txt' "Password" ];
          input
            ~at:
              At.
                [
                  id @@ Jstr.v "password";
                  name @@ Jstr.v "password";
                  type' @@ Jstr.v "password";
                ]
            ();
          (* Submit *)
          input
            ~at:
              At.
                [
                  id @@ Jstr.v "submit";
                  type' @@ Jstr.v "submit";
                  value @@ Jstr.v "Login";
                ]
            ();
        ])
  in
  El.
    [
      h1 [ txt' "Login" ];
      p [ txt' "You may login using any XMPP account." ];
      Evr.on_el ~default:false Form.Ev.submit
        (fun ev ->
          let form_data =
            Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
          in

          let jid_value =
            Form.Data.find form_data (Jstr.v "jid") |> Option.get
          in
          let password_value =
            Form.Data.find form_data (Jstr.v "password") |> Option.get
          in

          let jid =
            match jid_value with
            | `String js -> Jstr.to_string js |> Xmpp.Jid.of_string_exn
            | _ -> failwith "We need better error handling"
          in

          let password =
            match password_value with
            | `String js -> Jstr.to_string js
            | _ -> failwith "We need better error handling"
          in

          send_msg @@ Login (jid, password))
        login_form;
    ]

let view send_msg = function
  | Init ->
      El.
        [
          div ~at:At.[ class' @@ Jstr.v "text-content" ] @@ login_view send_msg;
        ]
  | Loading ->
      El.[ div ~at:At.[ class' @@ Jstr.v "text-content" ] [ txt' "Loading" ] ]
  | Client _ ->
      El.[ div ~at:At.[ class' @@ Jstr.v "text-content" ] [ txt' "Client" ] ]

(* let update model action =
 *   match action with
 *   | Login (jid, password) ->
 *       return @@ Client client
 *   | NoOp -> return model *)
