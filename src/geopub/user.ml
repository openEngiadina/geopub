(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* Manage user session *)

open Lwt
open Lwt.Syntax
open Lwt_react
open Brr
open Brr_io
open Brr_react
open Archi_lwt

(* Setup logging *)
let src = Logs.Src.create "GeoPub.User"

module Log = (val Logs.src_log src : Logs.LOG)

(* Component *)

type t = { database : Database.t; xmpp : Xmpp.t }

let start () database xmpp _router = return_ok { xmpp; database }
let stop _ = return_unit

let component =
  Component.using ~start ~stop
    ~dependencies:[ Database.component; Xmpp.component; Router.component ]

(* View *)

let login t =
  let login_form =
    El.(
      form
        [
          p
            [
              txt'
                "Currently not connected. You may login using any XMPP account.";
            ];
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
  El.(
    div
      [
        Evf.on_el ~default:false Form.Ev.submit
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
              | `String js -> Jstr.to_string js |> Xmppl.Jid.of_string_exn
              | _ -> failwith "We need better error handling"
            in

            let password =
              match password_value with
              | `String js -> Jstr.to_string js
              | _ -> failwith "We need better error handling"
            in

            ignore @@ Xmpp.(Connection.login (connection t.xmpp) ~password jid))
          login_form;
        p
          [
            Evf.on_el ~propagate:false Ev.click (fun _ev ->
                ignore
                @@ Xmpp.(Connection.login_anonymous_demo (connection t.xmpp)))
            @@ button [ txt' "Login anonymously with demo.opengiadina.net" ];
          ];
        p
          [
            Evf.on_el ~propagate:false Ev.click (fun _ev ->
                ignore
                @@ Xmpp.(
                     Connection.login (connection t.xmpp)
                       ~options:
                         {
                           ws_endpoint =
                             Some "ws://localhost:5280/xmpp-websocket";
                         }
                       ~password:"pencil"
                       (Jid.of_string_exn "user@strawberry.local")))
            @@ button [ txt' "dev login" ];
          ];
      ])

let view t =
  Xmpp.(Connection.client_signal (connection t.xmpp))
  |> S.map_s (function
       | Loadable.Idle -> return @@ [ login t ]
       | Loadable.Loading -> return El.[ txt' "Connecting..." ]
       | Loadable.Loaded client ->
           let* jid = Xmpp.Client.jid client in
           return
           @@ El.
                [
                  div [ p [ txt' @@ "Connected as " ^ Xmpp.Jid.to_string jid ] ];
                ]
       | Loadable.Error exn ->
           return El.[ txt' @@ "Error: " ^ Printexc.to_string exn ])
