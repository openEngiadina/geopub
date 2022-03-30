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

type t = {
  client : Client.t;
  state : Client.state;
  ec_responder : unit E.t;
  roster : Roster.roster S.t;
}

(* Initialization *)

let start_ec_responder client =
  (* Initiate Entity Cababilities (XEP-0115) responder *)
  Entity_capabilities.advertise ~category:"client" ~type':"web" ~name:"GeoPub"
    ~node:"https://codeberg.org/openEngiadina/geopub"
    [
      "http://jabber.org/protocol/caps";
      "urn:xmpp:microblog:0";
      "urn:xmpp:microblog:0+notify";
      "net.openengiadina.xmpp.activitystreams";
      "net.openengiadina.xmpp.activitystreams+notify"
      (* "http://jabber.org/protocol/geoloc"; *)
      (* "http://jabber.org/protocol/geoloc+notify"; *);
    ]
    client
  (* ignore the JIDs of who made queried our capabilities *)
  >|= fun e -> E.stamp e ()

let connect client =
  Lwt_result.(
    catch @@ Client.connect client >>= fun _ ->
    let* ec_responder = start_ec_responder client in
    let* roster = Roster.roster client in

    return { client; state = Client.Disconnected; ec_responder; roster })

let login jid password =
  Client.create Xmppl_websocket.default_options
    ~credentials:(`JidPassword (jid, password))
  >>= connect

let login_dev () =
  let jid = "user@strawberry.local" |> Xmppl.Jid.of_string_exn in
  Client.create
    { ws_endpoint = Some "ws://localhost:5280/xmpp-websocket" }
    ~credentials:(`JidPassword (jid, "pencil"))
  >>= connect

let login_anonymous_demo () =
  Client.create
    { ws_endpoint = Some "wss://openengiadina.net/xmpp-websocket" }
    ~credentials:(`Anonymous "demo.openengiadina.net")
  >>= connect

(* Subscription *)

let subscriptions model =
  E.select
    [
      S.changes @@ Client.state model.client
      |> E.map (fun state -> `XmppStateUpdate state);
      Client.stanzas model.client |> E.map (fun stanza -> `XmppStanza stanza);
      S.changes @@ model.roster
      |> E.map (fun roster -> `XmppRosterUpdate roster);
    ]

(* Roster management *)

let roster_add xmpp jid = Roster.add_update xmpp.client jid >|= fun _ -> `NoOp

let display_name xmpp jid =
  Option.bind
    (Jid.Map.find_opt (Jid.bare jid) (S.value xmpp.roster))
    (fun (item : Roster.Item.t) -> item.name)
  |> Option.value ~default:(Jid.to_string @@ Jid.bare jid)

(* PubSub *)

let publish_activitystreams jid xmpp id xml =
  let item =
    Xmlc.Tree.make_element
      ~attributes:[ (("", "id"), Rdf.Iri.to_string id) ]
      ~children:[ xml ] (Pubsub.Ns.pubsub "item")
  in
  Pubsub.publish ~to':(Jid.bare jid)
    ~node:"net.openengiadina.xmpp.activitystreams" xmpp.client (Some item)

(* Presence subscription management *)

let presence_subscribe xmpp jid =
  Roster.presence_subscribe xmpp.client jid >|= fun _ -> `NoOp

let presence_unsubscribe xmpp jid =
  Roster.presence_unsubscribe xmpp.client jid >|= fun _ -> `NoOp

let approve_presence_subscription xmpp jid =
  Roster.approve_presence_subscription xmpp.client jid >|= fun _ -> `NoOp

let deny_presence_subscription xmpp jid =
  Roster.deny_presence_subscription xmpp.client jid >|= fun _ -> `NoOp
