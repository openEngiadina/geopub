(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* This module provides the GeoPub XMPP functionality *)

open Lwt
open Lwt.Syntax
open Lwt_react

(* Modules for XMPP and various XEPs *)

module Jid = Xmppl.Jid
module Stanza = Xmppl.Stanza
module Client = Xmppl_websocket.Client
module Entity_capabilities = Xmppl_entity_capabilities.Make (Client)
module Roster = Xmppl_roster.Make (Client)
module Pubsub = Xmppl_pubsub.Make (Client)

(* XMPP client state *)

type model = {
  client : Client.t;
  state_s : Client.state S.t;
  stanza_listener : unit E.t;
  ec_responder : unit E.t;
}

let state model = S.value model.state_s

(* Initialize XMPP session and listen for stanzas *)

let start_ec_responder client =
  (* Initiate Entity Cababilities (XEP-0115) responder *)
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
  (* ignore the JIDs of who made queried our capabilities *)
  >|= fun e -> E.stamp e ()

(* Authentication *)

let connect send_msg client =
  ignore send_msg;
  Lwt_result.(
    catch @@ Client.connect client >>= fun _ ->
    let* ec_responder = start_ec_responder client in
    return
      {
        client;
        state_s = Client.state client;
        stanza_listener =
          Client.stanzas client
          |> E.map (fun stanza -> send_msg @@ `XmppStanza stanza);
        ec_responder;
      })
  >|= fun r -> `XmppLoginResult r

let login send_msg jid password =
  Client.create Xmppl_websocket.default_options
    ~credentials:(`JidPassword (jid, password))
  >>= connect send_msg

let login_dev send_msg () =
  let jid = "user@strawberry.local" |> Xmppl.Jid.of_string_exn in
  Client.create
    { ws_endpoint = Some "ws://localhost:5280/xmpp-websocket" }
    ~credentials:(`JidPassword (jid, "pencil"))
  >>= connect send_msg

let login_anonymous_demo send_msg () =
  Client.create
    { ws_endpoint = Some "wss://openengiadina.net/xmpp-websocket" }
    ~credentials:(`Anonymous "demo.openengiadina.net")
  >>= connect send_msg

(* Roster management *)

let roster client = Roster.roster client
let roster_add client jid = Roster.add_update client jid

(* Presence subscription management *)

let presense_subscribe = Roster.presence_subscribe
let presence_unsubscribe = Roster.presence_unsubscribe
let approve_presence_subscription = Roster.approve_presence_subscription
let deny_presence_subscription = Roster.deny_presence_subscription
