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

module Entity_capabilities = Xmppl_entity_capabilities.Make (Client)
module Roster = Xmppl_roster.Make (Client)
module Pubsub = Xmppl_pubsub.Make (Client)

(* Sub-Components *)

module Connection = Connection

(* Main component *)

type t = { connection : Connection.t }

let start () connection =
  Log.info (fun m -> m "XMPP component started");
  return_ok { connection }

let stop _ = return_unit

let component =
  Component.using ~start ~stop ~dependencies:[ Connection.component ]

(* Connection *)

(* let start_ec_responder client =
 *   (\* Initiate Entity Cababilities (XEP-0115) responder *\)
 *   Entity_capabilities.advertise ~category:"client" ~type':"web" ~name:"GeoPub"
 *     ~node:"https://codeberg.org/openEngiadina/geopub"
 *     [
 *       "http://jabber.org/protocol/caps";
 *       (\* "urn:xmpp:microblog:0"; *\)
 *       (\* "urn:xmpp:microblog:0+notify"; *\)
 *       "net.openengiadina.xmpp.activitystreams";
 *       "net.openengiadina.xmpp.activitystreams+notify"
 *       (\* "http://jabber.org/protocol/geoloc"; *\)
 *       (\* "http://jabber.org/protocol/geoloc+notify"; *\);
 *     ]
 *     client
 *   (\* ignore the JIDs of who made queried our capabilities *\)
 *   >|= fun e -> E.stamp e ()
 * 
 * let connect client =
 *   Lwt_result.(
 *     catch @@ Client.connect client >>= fun _ ->
 *     let* ec_responder = start_ec_responder client in
 *     let stanza_forwarder = Client.stanzas client |> E.map push_stanza in
 *     let handler = E.select [ ec_responder; stanza_forwarder ] in
 * 
 *     return { client; handler }) *)

(* Parse Stanza as RDF *)

(* let rdf_of_stanza (stanza : Stanza.t) =
 *   let rdf_parser =
 *     Xmlc.Parser.(
 *       element (Pubsub.Namespace.event "event") (fun _ ->
 *           element (Pubsub.Namespace.event "items") (fun _ ->
 *               element (Pubsub.Namespace.event "item") (fun _ ->
 *                   Xmlc.Tree.parser >>| Xmlc.Tree.to_seq
 *                   >>| Rdf_xml.parse_to_graph))))
 *   in
 *   match stanza with
 *   | Stanza.Message message ->
 *       Xmlc.Tree.parse_trees rdf_parser (List.to_seq message.payload)
 *       |> Lwt_result.catch >|= Result.to_option
 *   | _ -> return_none
 * 
 * let user_iri xmpp =
 *   let* jid = Client.jid xmpp.client in
 *   ("xmpp:" ^ Jid.(to_string @@ bare jid)) |> Rdf.Iri.of_string |> return *)

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
