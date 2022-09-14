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

(* LocalStorage *)

let local_storage = Storage.local G.window

let get_stored_credentials () =
  let storage_jid =
    Storage.get_item local_storage (Jstr.v "GeoPub.jid") |> fun o ->
    Option.bind o (fun s -> Jstr.to_string s |> Xmpp.Jid.of_string)
  in

  let storage_password =
    Storage.get_item local_storage (Jstr.v "GeoPub.password")
    |> Option.map (fun s -> Jstr.to_string s)
  in

  match (storage_jid, storage_password) with
  | Some jid, Some password -> Some (jid, password)
  | _ -> None

let set_stored_credentials jid password =
  ignore
  @@ Storage.set_item local_storage (Jstr.v "GeoPub.jid")
       (Xmpp.Jid.to_string jid |> Jstr.of_string);
  ignore
  @@ Storage.set_item local_storage (Jstr.v "GeoPub.password")
       (Jstr.of_string password)

(* Component *)

type t = { database : Database.t; xmpp : Xmpp.t }

let start msg database xmpp _router =
  match get_stored_credentials () with
  | Some (jid, password) ->
      msg "Logging in...";
      Xmpp.(Connection.login (connection xmpp) ~password jid)
      |> Lwt_result.catch
      >|= fun _ -> Ok { xmpp; database }
  | None -> return_ok { xmpp; database }

let stop _ = return_unit

let component =
  Component.using ~start ~stop
    ~dependencies:[ Database.component; Xmpp.component; Router.component ]

let jid t =
  Xmpp.(Connection.client_signal @@ connection t.xmpp)
  |> S.map Loadable.to_option
  |> S.map_s (function
       | Some client -> Xmpp.Client.jid client >|= Option.some
       | None -> return_none)

(* View *)

let login ?error t =
  let login_form =
    El.(
      form ~at:[ UIKit.Form.stacked ]
        [
          (* JID *)
          div ~at:[ UIKit.margin ]
            [
              label
                ~at:At.[ UIKit.Form.label; for' @@ Jstr.v "jid" ]
                [ txt' "JID" ];
              input
                ~at:
                  At.
                    [
                      UIKit.Form.input;
                      UIKit.Form.controls;
                      id @@ Jstr.v "jid";
                      name @@ Jstr.v "jid";
                      type' @@ Jstr.v "text";
                    ]
                ();
            ];
          (* Password *)
          div ~at:[ UIKit.margin ]
            [
              label
                ~at:At.[ UIKit.Form.label; for' @@ Jstr.v "password" ]
                [ txt' "Password" ];
              input
                ~at:
                  At.
                    [
                      UIKit.Form.input;
                      UIKit.Form.controls;
                      id @@ Jstr.v "password";
                      name @@ Jstr.v "password";
                      type' @@ Jstr.v "password";
                    ]
                ();
            ];
          (* Submit *)
          div ~at:[ UIKit.margin ]
            [
              input
                ~at:
                  At.
                    [
                      UIKit.Form.input;
                      UIKit.Button.primary;
                      id @@ Jstr.v "submit";
                      type' @@ Jstr.v "submit";
                      value @@ Jstr.v "Login";
                    ]
                ();
            ];
        ])
  in
  El.(
    div
      ~at:[ UIKit.container; UIKit.margin ]
      [
        h1 [ txt' "Login" ];
        (match error with
        | Some exn ->
            div
              ~at:[ UIKit.Alert.danger; UIKit.alert ]
              [
                txt' "Could not login:"; pre [ txt' @@ Printexc.to_string exn ];
              ]
        | None ->
            div ~at:[ UIKit.alert ]
              [
                txt'
                  "You can login with any XMPP server that provides a \
                   WebSocket endpoint.";
              ]);
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

            ignore
            @@ Xmpp.(
                 Connection.login (connection t.xmpp) ~password jid
                 |> Lwt_result.catch
                 >>= function
                 | Ok () -> return @@ set_stored_credentials jid password
                 | _ -> return_unit))
          login_form;
        div ~at:[ UIKit.margin ]
          [
            p
              [
                Evf.on_el ~propagate:false Ev.click (fun _ev ->
                    ignore
                    @@ Xmpp.(
                         Connection.login_anonymous_demo (connection t.xmpp)))
                @@ button
                     ~at:[ UIKit.Align.right; UIKit.button; UIKit.Button.link ]
                     [ txt' "Demo (anonymous login)" ];
              ];
          ];
        div
          ~at:
            (* Hack to hide it when log level is not set to debug (aka dev mode) *)
            At.(add_if (Logs.level () = Some Logs.Info) hidden [ UIKit.margin ])
          [
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
                @@ button
                     ~at:[ UIKit.Align.right; UIKit.button; UIKit.Button.link ]
                     [ txt' "Local development login" ];
              ];
          ];
      ])

let view t =
  Xmpp.(Connection.client_signal (connection t.xmpp))
  |> S.map_s (function
       | Loadable.Idle -> return @@ [ login t ]
       | Loadable.Loading ->
           return
             El.
               [
                 div
                   ~at:[ UIKit.container; UIKit.margin; UIKit.Align.center ]
                   [ div ~at:[ UIKit.spinner ] []; p [ txt' "Connecting..." ] ];
               ]
       | Loadable.Loaded client ->
           let* jid = Xmpp.Client.jid client in
           return
           @@ El.
                [
                  div
                    ~at:[ UIKit.container; UIKit.margin; UIKit.Align.center ]
                    [
                      h1 [ (txt' @@ Xmpp.Jid.(to_string @@ bare jid)) ];
                      div
                        [
                          p
                            [
                              txt' @@ "Connected as "
                              ^ Xmpp.Jid.(to_string @@ bare jid)
                              ^ ".";
                            ];
                        ];
                      h2 [ txt' "Logout" ];
                      div
                        ~at:[ UIKit.alert; UIKit.Alert.warning ]
                        [
                          p
                            [
                              txt'
                                "Logout will cause all data in the database to \
                                 be deleted.";
                            ];
                        ];
                      Evf.on_el ~default:false Ev.click (fun _ev ->
                          Log.info (fun m ->
                              m "Reseting database and logging out.");

                          ignore
                            (let* () = Database.delete t.database in
                             Storage.clear local_storage;
                             Brr.Window.reload G.window;
                             return_unit))
                      @@ button
                           ~at:[ UIKit.button; UIKit.Button.secondary ]
                           [ txt' @@ "Logout" ];
                    ];
                ]
       | Loadable.Error exn -> return @@ [ login ~error:exn t ])
