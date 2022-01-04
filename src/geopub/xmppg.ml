(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* This module provides the GeoPub XMPP functionality (Xmppg is short for XMPP GeoPub) *)

open Lwt
open Lwt.Syntax
open Lwt_react
open Reactor
open Reactor_brr
open Brr
open Brr_io
module L = Loadable

(* XMPP and various XEPs *)

module Client = Xmpp_websocket.Client
module Entity_capabilities = Xmpp_entity_capabilities.Make (Client)
module Roster = Xmpp_roster.Make (Client)
module Pubsub = Xmpp_pubsub.Make (Client)

type model = {
  client : Client.t;
  (* The bound Jid. Keep it here so we don't have to do Lwt calls to get it. *)
  jid : Xmpp.Jid.t;
  (* Lwt thread that listens for XMPP stanzas and handles them *)
  listener : unit event;
  roster : Roster.roster;
}

let contact_display_name model jid =
  Option.bind (Xmpp.Jid.Map.find_opt jid model.roster) (fun roster_item ->
      roster_item.name)
  |> Option.value ~default:Xmpp.Jid.(jid |> bare |> to_string)

type t = model L.t

type msg =
  | NoOp
  | Login of Xmpp.Jid.t * string
  | LoginAnonymousDemo
  | Authenticated of Client.t
  | ConnectionError of exn
  | Initialized of model
  | Logout
  (* Roster management *)
  | RosterPush of Roster.roster
  | AddContact of Xmpp.Jid.t
  (* Subscription management *)
  | PresenceSubscribe of Xmpp.Jid.t
  | PresenceUnsubscribe of Xmpp.Jid.t
  | PresenceApproveSubscription of Xmpp.Jid.t
  | PresenceDenySubscription of Xmpp.Jid.t
  (* Handle incoming and outgoing (already sent) messages *)
  | ReceiveMsg of Xmpp.Stanza.Message.t
  (* Publish a post *)
  | PublishPost of (Atom.Author.t -> id:string -> Atom.Entry.t)

let jid model = model.jid |> Xmpp.Jid.bare |> Xmpp.Jid.to_string
let jid_opt = function L.Loaded model -> Option.some @@ jid model | _ -> None
let init () = L.Idle |> Return.singleton
(* |> Return.command
 *    @@ Lwt.return
 *         (Login (Xmpp.Jid.of_string_exn "user@strawberry.local", "pencil")) *)

let login jid password =
  let* client =
    Client.create
      Xmpp_websocket.default_options
      (* {
       *   ws_endpoint =
       *     (\* use local prosody for development *\)
       *     Some "ws://localhost:5280/xmpp-websocket";
       * } *)
      ~credentials:(`JidPassword (jid, password))
  in
  let* connected = Lwt_result.catch @@ Client.connect client in
  match connected with
  | Ok () -> return @@ `XmppMsg (Authenticated client)
  | Error e -> return @@ `XmppMsg (ConnectionError e)

let login_anonymous_demo () =
  let* client =
    Client.create
      { ws_endpoint = Some "wss://openengiadina.net/xmpp-websocket" }
      ~credentials:(`Anonymous "demo.openengiadina.net")
  in
  let* connected = Lwt_result.catch @@ Client.connect client in
  match connected with
  | Ok () -> return @@ `XmppMsg (Authenticated client)
  | Error e -> return @@ `XmppMsg (ConnectionError e)

let xmpp_init ~send_msg client =
  let* jid = Client.jid client in
  let* ec_responder =
    Entity_capabilities.advertise ~category:"client" ~type':"web" ~name:"GeoPub"
      ~node:"https://codeberg.org/openEngiadina/geopub"
      [
        "http://jabber.org/protocol/caps";
        "urn:xmpp:microblog:0";
        "urn:xmpp:microblog:0+notify"
        (* "http://jabber.org/protocol/geoloc"; *)
        (* "http://jabber.org/protocol/geoloc+notify"; *);
      ]
      client
  in
  let msg_listener =
    Client.stanzas client
    |> E.map (fun stanza ->
           match stanza with
           | Xmpp.Stanza.Message msg -> send_msg @@ `XmppMsg (ReceiveMsg msg)
           | _ -> ())
  in
  let* roster_s = Roster.roster client in
  let roster = roster_s |> S.value in
  let listener =
    E.merge
      (fun _ _ -> ())
      ()
      [
        E.stamp ec_responder ();
        msg_listener;
        (* Listen for roster_pushes *)
        roster_s |> S.changes
        |> E.map (fun roster -> send_msg @@ `XmppMsg (RosterPush roster));
      ]
  in
  return @@ `XmppMsg (Initialized { jid; client; listener; roster })

let make_outgoing_message client to' message =
  let* from = Client.jid client in
  return
  @@ Xmpp.Stanza.Message.make ~to' ~from ~type':"chat"
       Xmlc.Tree.
         [
           make_element ~children:[ make_data message ] (Xmpp.Ns.client "body");
         ]

let send_xmpp_message client message =
  let* () = Client.send_message client message in
  return @@ `NoOp

(* TODO move to Xmpp.Jid.to_uri *)
let uri_of_jid jid = "xmpp:" ^ (jid |> Xmpp.Jid.bare |> Xmpp.Jid.to_string)

let update ~send_msg model msg =
  match (model, msg) with
  (* Initiate authentication *)
  | _, Login (jid, password) ->
      L.Loading |> Return.singleton |> Return.command (login jid password)
  | _, LoginAnonymousDemo ->
      L.Loading |> Return.singleton |> Return.command (login_anonymous_demo ())
  | _, ConnectionError _ -> L.Idle |> Return.singleton
  | _, Authenticated client ->
      L.Loading |> Return.singleton
      |> Return.command (xmpp_init ~send_msg client)
  (* Authentication succeeded *)
  | _, Initialized model' -> L.Loaded model' |> Return.singleton
  | L.Loaded model, Logout ->
      L.Idle |> Return.singleton
      |> Return.command (Client.disconnect model.client >|= fun _ -> `NoOp)
  (* Roster management *)
  | L.Loaded model, RosterPush roster ->
      L.Loaded { model with roster } |> Return.singleton
  | L.Loaded model, AddContact jid ->
      L.Loaded model |> Return.singleton
      |> Return.command
           ( Roster.add_update model.client jid >|= fun _ ->
             `SetRoute (Route.Roster jid) )
  (* Subscription Management *)
  | L.Loaded model, PresenceSubscribe jid ->
      L.Loaded model |> Return.singleton
      |> Return.command
           (Roster.presence_subscribe model.client jid >|= fun _ -> `NoOp)
  | L.Loaded model, PresenceUnsubscribe jid ->
      L.Loaded model |> Return.singleton
      |> Return.command
           (Roster.presence_unsubscribe model.client jid >|= fun _ -> `NoOp)
  | L.Loaded model, PresenceApproveSubscription jid ->
      L.Loaded model |> Return.singleton
      |> Return.command
           ( Roster.approve_presence_subscription model.client jid >|= fun _ ->
             `NoOp )
  | L.Loaded model, PresenceDenySubscription jid ->
      L.Loaded model |> Return.singleton
      |> Return.command
           ( Roster.deny_presence_subscription model.client jid >|= fun _ ->
             `NoOp )
  (* Handle incoming message *)
  | L.Loaded model, ReceiveMsg msg ->
      L.Loaded model |> Return.singleton
      |> Return.command (return @@ `ReceiveMessage msg)
  | L.Loaded model, PublishPost make_atom ->
      let author =
        Atom.Author.make ~uri:(uri_of_jid model.jid)
          (Option.value ~default:"blups" model.jid.local)
      in
      let item_id = Client.generate_id model.client in
      let atom_entry = make_atom author ~id:item_id in
      let item =
        Xmlc.Tree.make_element
          ~attributes:[ (("", "id"), item_id) ]
          ~children:[ Atom.Entry.to_xml atom_entry ]
          (Pubsub.Ns.pubsub "item")
      in
      L.Loaded model |> Return.singleton
      |> Return.command
           ( Pubsub.publish ~to':(Xmpp.Jid.bare model.jid)
               ~node:"urn:xmpp:microblog:0" model.client (Option.some @@ item)
           >|= fun _ -> `NoOp )
  | _, _ -> model |> Return.singleton

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
      p
        [
          Evr.on_el Ev.click (fun _ -> send_msg (`XmppMsg LoginAnonymousDemo))
          @@ a
               ~at:At.[ href @@ Jstr.v "#" ]
               [ txt' "Login anonymously with demo.opengiadina.net" ];
        ];
    ]

let account_view send_msg = function
  | L.Idle ->
      El.
        [
          div ~at:At.[ class' @@ Jstr.v "text-content" ] @@ login_view send_msg;
        ]
  | L.Loading ->
      El.[ div ~at:At.[ class' @@ Jstr.v "text-content" ] [ txt' "Loading" ] ]
  | L.Loaded model ->
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
