(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* This module provides the GeoPub XMPP functionality *)

open Lwt
open Archi_lwt

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Xmpp"

module Log = (val Logs.src_log src : Logs.LOG)

(* XMPP Modules *)

module Jid = Xmppl.Jid
module Stanza = Xmppl.Stanza
module Client = Xmppl_websocket.Client

(* XEPs *)

module Roster = Xmppl_roster.Make (Client)
module Pubsub = Xmppl_pubsub.Make (Client)

(* Sub-Components *)

module Connection = Connection

(* Main component *)

type t = {
  connection : Connection.t;
  entity_capabilities : Entity_capabilities.t;
}

let start () connection entity_capabilities =
  Log.info (fun m -> m "XMPP component started");
  return_ok { connection; entity_capabilities }

let stop _ = return_unit

let component =
  Component.using ~start ~stop
    ~dependencies:[ Connection.component; Entity_capabilities.component ]

let connection t = t.connection

(* Roster management *)

(* let roster_add xmpp jid = Roster.add_update xmpp.client jid >|= fun _ -> `NoOp *)

(* PubSub *)

(* let publish_activitystreams xmpp id xml =
 *   let* jid = Client.jid xmpp.client in
 *   let item =
 *     Xmlc.Tree.make_element
 *       ~attributes:[ (("", "id"), Rdf.Iri.to_string id) ]
 *       ~children:[ xml ]
 *       (Pubsub.Namespace.pubsub "item")
 *   in
 *   Pubsub.publish ~to':(Jid.bare jid)
 *     ~node:"net.openengiadina.xmpp.activitystreams" xmpp.client (Some item)
 * 
 * (\* Presence subscription management *\) *)

(* let presence_subscribe xmpp jid =
 *   Roster.presence_subscribe xmpp.client jid >|= fun _ -> `NoOp
 * 
 * let presence_unsubscribe xmpp jid =
 *   Roster.presence_unsubscribe xmpp.client jid >|= fun _ -> `NoOp
 * 
 * let approve_presence_subscription xmpp jid =
 *   Roster.approve_presence_subscription xmpp.client jid >|= fun _ -> `NoOp
 * 
 * let deny_presence_subscription xmpp jid =
 *   Roster.deny_presence_subscription xmpp.client jid >|= fun _ -> `NoOp *)
