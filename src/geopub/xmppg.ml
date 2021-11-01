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

type contact = {
  roster_item : Roster.Item.t option;
  messages : Xmpp.Stanza.Message.t list;
}

type model = {
  client : Client.t;
  (* Lwt thread that listens for XMPP stanzas and handles them *)
  listener : unit event;
  contacts : contact Xmpp.Jid.Map.t;
}

let contact_display_name model jid =
  Option.bind (Xmpp.Jid.Map.find_opt jid model.contacts) (fun contact ->
      match contact.roster_item with
      | Some roster_item -> roster_item.name
      | _ -> None)
  |> Option.value ~default:Xmpp.Jid.(jid |> bare |> to_string)

type t = model L.t

type msg =
  | NoOp
  | Login of Xmpp.Jid.t * string
  | Authenticated of model
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
  (* Send a message *)
  | SendMsg of Xmpp.Jid.t * string
  (* Publish a simple post *)
  | PublishPost of { title : string; content : string }

let jid model = Client.jid model.client |> Xmpp.Jid.bare |> Xmpp.Jid.to_string

let jid_opt = function L.Loaded model -> Option.some @@ jid model | _ -> None

let init () =
  L.Idle |> Return.singleton
  |> Return.command
     @@ Lwt.return
          (Login (Xmpp.Jid.of_string_exn "user@strawberry.local", "pencil"))

let login ~send_msg jid password =
  let* client =
    Client.create
      {
        ws_endpoint =
          (* use local prosody for development *)
          Some "ws://localhost:5280/xmpp-websocket";
      }
      jid ~password
  in
  let* () = Client.connect client in
  let* ec_responder =
    Entity_capabilities.advertise ~category:"client" ~type':"web" ~name:"GeoPub"
      ~node:"https://codeberg.org/openEngiadina/geopub"
      [
        "http://jabber.org/protocol/caps";
        "urn:xmpp:microblog:0";
        "urn:xmpp:microblog:0+notify";
        (* "http://jabber.org/protocol/geoloc"; *)
        (* "http://jabber.org/protocol/geoloc+notify"; *)
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
  let contacts =
    roster_s |> S.value
    |> Xmpp.Jid.Map.map (fun (roster_item : Roster.Item.t) ->
           { roster_item = Some roster_item; messages = [] })
  in
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
  return @@ `XmppMsg (Authenticated { client; listener; contacts })

let contacts_add_incoming_msg contacts (msg : Xmpp.Stanza.Message.t) =
  match msg.from with
  | Some from ->
      Xmpp.Jid.Map.update (Xmpp.Jid.bare from)
        (function
          | Some contact ->
              Some { contact with messages = msg :: contact.messages }
          | None -> Some { roster_item = None; messages = [ msg ] })
        contacts
  | None -> contacts

let contacts_add_outgoing_msg contacts (msg : Xmpp.Stanza.Message.t) =
  Xmpp.Jid.Map.update (Xmpp.Jid.bare msg.to')
    (function
      | Some contact -> Some { contact with messages = msg :: contact.messages }
      | None -> Some { roster_item = None; messages = [ msg ] })
    contacts

let make_outgoing_message client to' message =
  let from = Client.jid client in
  Xmpp.Stanza.Message.make ~to' ~from ~type':"chat"
    Xmlc.
      [ make_element ~children:[ make_text message ] (Xmpp.Ns.client "body") ]

let send_xmpp_message client message =
  let* () = Client.send_message client message in
  return @@ `NoOp

let uri_of_jid jid = "xmpp:" ^ (jid |> Xmpp.Jid.bare |> Xmpp.Jid.to_string)

let update ~send_msg model msg =
  match (model, msg) with
  (* Initiate authentication *)
  | _, Login (jid, password) ->
      L.Loading |> Return.singleton
      |> Return.command (login ~send_msg jid password)
  (* Authentication succeeded *)
  | _, Authenticated model' -> L.Loaded model' |> Return.singleton
  | L.Loaded model, Logout ->
      L.Idle |> Return.singleton
      |> Return.command (Client.disconnect model.client >|= fun _ -> `NoOp)
  (* Roster management *)
  | L.Loaded model, RosterPush roster ->
      let updated_contacts =
        Xmpp.Jid.Map.merge
          (fun _jid contact roster_item ->
            match (contact, roster_item) with
            | Some contact, _ -> Some { contact with roster_item }
            | None, Some roster_item ->
                Some { roster_item = Some roster_item; messages = [] }
            | _ -> None)
          model.contacts roster
      in
      L.Loaded { model with contacts = updated_contacts } |> Return.singleton
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
      L.Loaded
        { model with contacts = contacts_add_incoming_msg model.contacts msg }
      |> Return.singleton
  (* Send a message *)
  | L.Loaded ({ client; _ } as model), SendMsg (to', message) ->
      let message = make_outgoing_message client to' message in
      L.Loaded
        {
          model with
          contacts = contacts_add_outgoing_msg model.contacts message;
        }
      |> Return.singleton
      |> Return.command (send_xmpp_message client message)
  | L.Loaded model, PublishPost { content; title } ->
      let jid = Client.jid model.client in
      let author =
        Atom.Author.make ~uri:(uri_of_jid jid)
          (Option.value ~default:"blups" jid.local)
      in
      (* TODO figure out way to get a proper geoloc *)
      let geoloc = Geoloc.make 0. 0. in
      let item_id = Client.generate_id model.client in
      let atom_entry =
        Atom.Entry.make ~title ~content ~authors:[ author ] ~id:item_id ~geoloc
          ~updated:(Ptime_clock.now ()) ()
      in
      let jid = Client.jid model.client in
      let item =
        Xmlc.make_element
          ~attributes:[ (("", "id"), item_id) ]
          ~children:[ Atom.Entry.to_xml atom_entry ]
          (Pubsub.Ns.pubsub "item")
      in
      L.Loaded model |> Return.singleton
      |> Return.command
           ( Pubsub.publish ~to':jid ~node:"urn:xmpp:microblog:0" model.client
               (Option.some @@ item)
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
