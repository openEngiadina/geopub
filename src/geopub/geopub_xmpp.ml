(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* Lwt *)

open Lwt
open Lwt.Syntax

(* Reactor *)

open Reactor
open Reactor_brr

(* Brr *)

open Brr
open Brr_io

(* XMPP and various XEPs *)

module Client = Xmpp_websocket.Client
module Entity_capabilities = Xmpp_entity_capabilities.Make (Client)
module Roster = Xmpp_roster.Make (Client)

type model = {
  client : Client.t;
  (* Lwt thread that listens for XMPP stanzas and handles them *)
  listener : unit Lwt.t;
  roster : Roster.Item.t list;
}

type t = model Loadable.t

type msg = Login of Xmpp.Jid.t * string | Authenticated of model | Logout

let jid model = Client.jid model.client |> Xmpp.Jid.bare |> Xmpp.Jid.to_string

let jid_opt = function
  | Loadable.Loaded model -> Option.some @@ jid model
  | _ -> None

let init () =
  Loadable.Idle |> Return.singleton
  |> Return.command
     @@ Lwt.return
          (Login (Xmpp.Jid.of_string_exn "user@strawberry.local", "pencil"))

let login jid password =
  let* client =
    Client.create
      { ws_endpoint = Some "ws://localhost:5280/xmpp-websocket" }
      jid ~password
  in
  let* () = Client.connect client in
  let ec_responder =
    Entity_capabilities.advertise ~category:"client" ~type':"web" ~name:"GeoPub"
      [ "http://jabber.org/protocol/caps" ]
      client
  in
  let* roster = Roster.get client in
  let listener = Lwt.pick [ ec_responder ] in
  return @@ Authenticated { client; listener; roster }

let update ~send_msg _model msg =
  ignore send_msg;
  match msg with
  | Login (jid, password) ->
      Loadable.Loading |> Return.singleton
      |> Return.command (login jid password)
  | Authenticated model' -> Loadable.Loaded model' |> Return.singleton
  | Logout ->
      (* TODO implement Client.disconnect *)
      Loadable.Idle |> Return.singleton

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

          send_msg @@ `XmppMsg (Login (jid, password)))
        login_form;
    ]

let roster_sidebar send_msg model =
  let roster_item_el (item : Roster.Item.t) =
    El.(
      li
        ~at:At.[ class' @@ Jstr.v "roster-item" ]
        [
          Evr.on_el Ev.click (fun _ ->
              send_msg @@ `SetRoute (Route.Messages (Some item.jid)))
          @@ a
               ~at:At.[ href @@ Jstr.v "#" ]
               [ txt' @@ Xmpp.Jid.to_string item.jid ];
        ])
  in
  El.(
    div
      ~at:At.[ class' @@ Jstr.v "sidebar" ]
      [
        ul
          ~at:At.[ class' @@ Jstr.v "roster" ]
          List.(map roster_item_el model.roster);
      ])

let messages_view send_msg model _selected_jid =
  match model with
  | Loadable.Loaded model ->
      El.
        [
          roster_sidebar send_msg model;
          div ~at:At.[ class' @@ Jstr.v "messages" ] [];
        ]
  | _ ->
      El.
        [
          div ~at:At.[ class' @@ Jstr.v "text-content" ] @@ login_view send_msg;
        ]

let account_view send_msg = function
  | Loadable.Idle ->
      El.
        [
          div ~at:At.[ class' @@ Jstr.v "text-content" ] @@ login_view send_msg;
        ]
  | Loadable.Loading ->
      El.[ div ~at:At.[ class' @@ Jstr.v "text-content" ] [ txt' "Loading" ] ]
  | Loadable.Loaded model ->
      El.
        [
          div
            ~at:At.[ class' @@ Jstr.v "text-content" ]
            [
              h1 [ txt' "" ];
              p [ txt' @@ "Authenticated as " ^ jid model ];
              p
                [
                  Evr.on_el Ev.click (fun _ -> send_msg @@ `XmppMsg Logout)
                  @@ button [ txt' "Logout" ];
                ];
            ];
        ]
