(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Brr_io
open Lwt
open Lwt.Syntax

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Settings"

module Log = (val Logs.src_log src : Logs.LOG)

(* GeoPub components *)
module Database = Geopub_database
module Xmpp = Geopub_xmpp

let database_settings ~update (model : Model.t) =
  let* triple_count = Database.triple_count model.database in
  (* let triple_count = model.counter in *)
  return
    El.(
      div
        [
          h2 [ txt' "Database" ];
          p [ txt' ("Triple Count: " ^ string_of_int triple_count) ];
          p
            [
              Ui.on_el Ev.click (fun ev ->
                  (* Disable button and set text *)
                  let button = Ev.(target_to_jv @@ target ev) in
                  Jv.set button "disabled" Jv.true';
                  Jv.set button "textContent"
                  @@ Jv.of_string
                       "Resetting and reloading default vocabularies...";
                  update (fun (model : Model.t) ->
                      let* database = Database.reset model.database in
                      return { model with database }))
              @@ button [ txt' "Reset Database" ];
            ];
        ])

let xmpp_settings ~update (model : Model.t) =
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

  let* state_view =
    match model.xmpp with
    | Loadable.Idle ->
        return
        @@ El.(
             div
               [
                 Ui.on_el ~default:false Form.Ev.submit
                   (fun ev ->
                     let form_data =
                       Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv
                       @@ Ev.target ev
                     in

                     let jid_value =
                       Form.Data.find form_data (Jstr.v "jid") |> Option.get
                     in
                     let password_value =
                       Form.Data.find form_data (Jstr.v "password")
                       |> Option.get
                     in

                     let jid =
                       match jid_value with
                       | `String js ->
                           Jstr.to_string js |> Xmppl.Jid.of_string_exn
                       | _ -> failwith "We need better error handling"
                     in

                     let password =
                       match password_value with
                       | `String js -> Jstr.to_string js
                       | _ -> failwith "We need better error handling"
                     in

                     (* Disable form and set text *)
                     let form = Ev.(target_to_jv @@ target ev) in
                     Jv.set form "textContent" @@ Jv.of_string "Connecting...";

                     update (fun (model : Model.t) ->
                         let* xmpp =
                           Xmpp.login jid password >|= Loadable.of_result
                         in
                         return { model with xmpp }))
                   login_form;
                 p
                   [
                     Ui.on_el ~propagate:false Ev.click (fun ev ->
                         (* Disable form and set text *)
                         let form = Ev.(target_to_jv @@ target ev) in
                         Jv.set form "textContent"
                         @@ Jv.of_string "Connecting...";
                         update (fun (model : Model.t) ->
                             let* xmpp =
                               Xmpp.login_anonymous_demo ()
                               >|= Loadable.of_result
                             in
                             return { model with xmpp }))
                     @@ button
                          [ txt' "Login anonymously with demo.opengiadina.net" ];
                   ];
               ])
    | Loadable.Loading -> return El.(txt' "Connecting...")
    | Loadable.Loaded xmpp ->
        let* jid = Xmpp.Client.jid xmpp.client in
        return
        @@ El.(
             div
               [
                 p [ txt' @@ "Connected as " ^ Xmpp.Jid.to_string jid ];
                 Ui.on_el Ev.click (fun _ev ->
                     update (fun _model ->
                         let* () = Xmpp.Client.disconnect xmpp.client in
                         return { model with xmpp = Loadable.Idle }))
                 @@ button [ txt' "Disconnect" ];
               ])
    | Loadable.Error exn ->
        return El.(txt' @@ "Error: " ^ Printexc.to_string exn)
  in
  return
    El.(
      div
        [
          h2 [ txt' "XMPP" ];
          div ~at:At.[ class' @@ Jstr.v "login" ] [ state_view ];
        ])

let view ~update (model : Model.t) =
  let* database_settings = database_settings ~update model in
  let* xmpp_settings = xmpp_settings ~update model in
  return
    El.
      [
        Ui.geopub_menu model;
        div
          ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
          [ h1 [ txt' "Settings" ]; xmpp_settings; database_settings ];
      ]
