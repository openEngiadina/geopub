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

(* Stanza events *)

(* We need to create this global stanza event and pipe stanzas from dynamic clients. *)

let stanzas, push_stanza = E.create ()

(* XMPP client state *)

type t = {
  client : Client.t;
  (* We need to keep reference to this to prevent GC from stopping the XMPP process *)
  handler : unit E.t;
}

(* Connection *)

let start_ec_responder client =
  (* Initiate Entity Cababilities (XEP-0115) responder *)
  Entity_capabilities.advertise ~category:"client" ~type':"web" ~name:"GeoPub"
    ~node:"https://codeberg.org/openEngiadina/geopub"
    [
      "http://jabber.org/protocol/caps";
      (* "urn:xmpp:microblog:0"; *)
      (* "urn:xmpp:microblog:0+notify"; *)
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
    let stanza_forwarder = Client.stanzas client |> E.map push_stanza in
    let handler = E.select [ ec_responder; stanza_forwarder ] in

    return { client; handler })

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

(* Parse Stanza as RDF *)

let rdf_of_stanza (stanza : Stanza.t) =
  let rdf_parser =
    Xmlc.Parser.(
      element (Pubsub.Namespace.event "event") (fun _ ->
          element (Pubsub.Namespace.event "items") (fun _ ->
              element (Pubsub.Namespace.event "item") (fun _ ->
                  Xmlc.Tree.parser >>| Xmlc.Tree.to_seq
                  >>| Rdf_xml.parse_to_graph))))
  in
  match stanza with
  | Stanza.Message message ->
      Xmlc.Tree.parse_trees rdf_parser (List.to_seq message.payload)
      |> Lwt_result.catch >|= Result.to_option
  | _ -> return_none

let user_iri xmpp =
  let* jid = Client.jid xmpp.client in
  ("xmpp:" ^ Jid.(to_string @@ bare jid)) |> Rdf.Iri.of_string |> return

(* Roster management *)

let roster_add xmpp jid = Roster.add_update xmpp.client jid >|= fun _ -> `NoOp

(* PubSub *)

let publish_activitystreams xmpp id xml =
  let* jid = Client.jid xmpp.client in
  let item =
    Xmlc.Tree.make_element
      ~attributes:[ (("", "id"), Rdf.Iri.to_string id) ]
      ~children:[ xml ]
      (Pubsub.Namespace.pubsub "item")
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
