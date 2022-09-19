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

(* Dev *)

let dev_login xmpp =
  ignore
  @@ Xmpp.(
       Connection.login (connection xmpp)
         ~options:{ ws_endpoint = Some "ws://localhost:5280/xmpp-websocket" }
         ~password:"pencil"
         (Jid.of_string_exn "user@strawberry.local"))

(* Component *)

type t = {
  database : Database.t;
  xmpp : Xmpp.t;
  roster : Xmpp.Roster.t;
  router : Router.t;
}

let start msg database xmpp roster router =
  (* Automatically login when in dev mode *)
  (* dev_login xmpp; *)
  match get_stored_credentials () with
  | Some (jid, password) ->
      msg "Logging in...";
      Xmpp.(Connection.login (connection xmpp) ~password jid)
      |> Lwt_result.catch
      >|= fun _ -> Ok { xmpp; database; roster; router }
  | None -> return_ok { xmpp; database; roster; router }

let stop _ = return_unit

let component =
  Component.using ~start ~stop
    ~dependencies:
      [
        Database.component;
        Xmpp.component;
        Xmpp.Roster.component;
        Router.component;
      ]

let jid t =
  Xmpp.(Connection.client_signal @@ connection t.xmpp)
  |> S.map Loadable.to_option
  |> S.map_s (function
       | Some client -> Xmpp.Client.jid client >|= Option.some
       | None -> return_none)

(* Roster *)

module Roster = struct
  let subscription_from (item : Xmpp.Roster.Item.t) =
    let subscription = item.subscription |> Option.value ~default:"none" in
    (* contact is pre-approved *)
    item.approved |> Option.value ~default:false
    (* contact is subscribed *)
    || subscription = "from"
    || subscription = "both"

  let subscription_to (item : Xmpp.Roster.Item.t) =
    let subscription = item.subscription |> Option.value ~default:"none" in
    let ask = item.ask |> Option.value ~default:"none" in
    subscription = "to" || subscription = "both" || ask = "subscribe"

  let add_form client =
    El.(
      div
        ~at:[ UIKit.section; UIKit.padding ]
        [
          Evf.on_el ~default:false Form.Ev.submit (fun ev ->
              let form_data =
                Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv
                @@ Ev.target ev
              in

              let jid_value =
                Form.Data.find form_data (Jstr.v "jid") |> Option.get
              in

              let jid =
                match jid_value with
                | `String js -> Jstr.to_string js |> Xmppl.Jid.of_string_exn
                | _ -> failwith "We need better error handling"
              in

              ignore @@ Xmpp.Roster.add_update client jid)
          @@ form
               ~at:[ UIKit.grid; UIKit.margin; UIKit.width 1 2 ]
               [
                 input
                   ~at:
                     At.
                       [
                         UIKit.Form.input;
                         type' @@ Jstr.v "text";
                         placeholder @@ Jstr.v "JID";
                         name @@ Jstr.v "jid";
                         UIKit.width 2 3;
                       ]
                   ();
                 input
                   ~at:
                     [
                       UIKit.Form.input;
                       UIKit.Button.primary;
                       UIKit.width 1 3;
                       At.type' @@ Jstr.v "submit";
                       At.value @@ Jstr.v "Add";
                     ]
                   ();
               ];
        ])

  let view client roster =
    S.map
      (fun roster ->
        El.(
          ul ~at:[ UIKit.Comment.list ]
            (Xmpp.Jid.Map.to_seq roster |> Seq.map snd
            |> Seq.map (fun (item : Xmpp.Roster.Item.t) ->
                   li ~at:[ UIKit.comment ]
                     [
                       header ~at:[ UIKit.Comment.header ]
                         [
                           span ~at:[ UIKit.Comment.title ]
                             [ txt' @@ Xmpp.Jid.to_string item.jid ];
                           ul
                             ~at:
                               [
                                 UIKit.Comment.meta;
                                 UIKit.subnav;
                                 UIKit.Subnav.divider;
                               ]
                             [
                               li
                                 [
                                   txt' "Publish to: ";
                                   Evf.on_el Ev.click (fun ev ->
                                       let target =
                                         Ev.(target_to_jv @@ target ev)
                                       in
                                       let checked =
                                         Jv.(to_bool @@ get target "checked")
                                       in
                                       ignore
                                       @@
                                       if checked then
                                         Xmpp.Roster
                                         .approve_presence_subscription client
                                           item.jid
                                       else
                                         Xmpp.Roster.deny_presence_subscription
                                           client item.jid)
                                   @@ input
                                        ~at:
                                          At.(
                                            add_if (subscription_from item)
                                              checked
                                              [ type' @@ Jstr.v "checkbox" ])
                                        ();
                                 ];
                               li
                                 [
                                   txt' "Receive from: ";
                                   Evf.on_el Ev.click (fun ev ->
                                       let target =
                                         Ev.(target_to_jv @@ target ev)
                                       in
                                       let checked =
                                         Jv.(to_bool @@ get target "checked")
                                       in
                                       ignore
                                       @@
                                       if checked then
                                         Xmpp.Roster.presence_subscribe client
                                           item.jid
                                       else
                                         Xmpp.Roster.presence_unsubscribe client
                                           item.jid)
                                   @@ input
                                        ~at:
                                          At.(
                                            add_if (subscription_to item)
                                              checked
                                              [ type' @@ Jstr.v "checkbox" ])
                                        ();
                                 ];
                               li
                                 [
                                   Evf.on_el Ev.click (fun _ev ->
                                       ignore
                                       @@ Xmpp.Roster.remove client item.jid)
                                   @@ a [ txt' "Remove" ];
                                 ];
                               li
                                 [
                                   a
                                     ~at:
                                       At.
                                         [
                                           title @@ Jstr.v "Send direct message";
                                           UIKit.Icon.mail;
                                           href
                                           @@ Jstr.v
                                                ("xmpp:"
                                                ^ Xmpp.Jid.to_string item.jid);
                                         ]
                                     [];
                                 ];
                             ];
                         ];
                     ])
            |> List.of_seq)))
      roster
end

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
                 | Ok () ->
                     set_stored_credentials jid password;
                     Router.set_route t.router (Route.Activity None);
                     return_unit
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
                    dev_login t.xmpp)
                @@ button
                     ~at:[ UIKit.Align.right; UIKit.button; UIKit.Button.link ]
                     [ txt' "Local development login" ];
              ];
          ];
      ])

let delay t v =
  let* () = Js_of_ocaml_lwt.Lwt_js.sleep t in
  return v

let view t =
  Xmpp.(Connection.client_signal (connection t.xmpp)) |> fun s ->
  S.bind_s s (function
    | Loadable.Idle -> return @@ S.const @@ [ login t ]
    | Loadable.Loading ->
        return
        @@ S.const
             El.
               [
                 div
                   ~at:[ UIKit.container; UIKit.margin; UIKit.Align.center ]
                   [ div ~at:[ UIKit.spinner ] []; p [ txt' "Connecting..." ] ];
               ]
    | Loadable.Loaded client ->
        let* jid = Xmpp.Client.jid client in
        Roster.view client t.roster
        (* HACK HACK HACK: Somewhere we incorrectly use React (some
           signal not initiated). Delay by a bit to not make
           break. Basically, I'm too stupid to use React propery. Need to
           find another way of doing reactive programming. *)
        |> delay 0.2
        >|= S.map (fun roster_el ->
                El.
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
                        h2 [ txt' "Roster" ];
                        roster_el;
                        Roster.add_form client;
                        h2 [ txt' "Logout" ];
                        div
                          ~at:[ UIKit.alert; UIKit.Alert.warning ]
                          [
                            p
                              [
                                txt'
                                  "Logout will cause all data in the database \
                                   to be deleted.";
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
                  ])
    | Loadable.Error exn -> return @@ S.const [ login ~error:exn t ])
