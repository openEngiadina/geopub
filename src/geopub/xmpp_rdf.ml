(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* RDF over XMPP

   - Receive RDF from XMPP stanzas and add to Datbase
   - Publish RDF over XMPP
*)

open Lwt
open Lwt.Syntax
open Lwt_react
open Archi_lwt

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Xmpp_rdf"

module Log = (val Logs.src_log src : Logs.LOG)

(* XMPP Modules *)

module Jid = Xmppl.Jid
module Stanza = Xmppl.Stanza
module Client = Xmppl_websocket.Client
module Pubsub = Xmppl_pubsub.Make (Client)

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

(* Component *)

type t = { xmpp : Xmpp.t; listener : unit event }

let listen xmpp database =
  Xmpp.connection xmpp |> Xmpp.Connection.client_signal |> S.changes
  |> E.fmap (function
       | Loadable.Loaded client ->
           Client.stanzas client
           |> E.map_s (fun stanza ->
                  let* rdf_opt = rdf_of_stanza stanza in
                  match rdf_opt with
                  | Some graph ->
                      Log.debug (fun m ->
                          m "GeoPub received RDF over XMPP: %a" Rdf.Graph.pp
                            graph);
                      Database.add_graph database graph
                  | None -> return_unit)
           |> Option.some
       | _ -> None)
  |> E.switch E.never

let start (_ : string -> unit) xmpp database =
  let listener = listen xmpp database in
  return_ok { xmpp; listener }

let stop _ = return_unit

let component =
  Component.using ~start ~stop
    ~dependencies:[ Xmpp.component; Database.component ]

(* Publish RDF over XMPP *)

module Publish = struct
  let rdf_to_xml rdf =
    let prefixes =
      [ ("as", Namespace.activitystreams ""); ("geo", Namespace.geo "") ]
    in
    let signals = rdf |> Rdf_xml.to_signals ~prefixes in
    let stream = Lwt_stream.of_seq signals in
    Xmlc.Parser.parse_stream Xmlc.Tree.parser stream

  let to_activitystreams_pep t id graph =
    let* client =
      Xmpp.connection t.xmpp |> Xmpp.Connection.client |> function
      | Ok client -> return client
      | _ -> fail_with "no XMPP client"
    in
    let* jid = Xmpp.Client.jid client in
    let* xml = rdf_to_xml graph in
    let item =
      Xmlc.Tree.make_element
        ~attributes:[ (("", "id"), Rdf.Iri.to_string id) ]
        ~children:[ xml ]
        (Pubsub.Namespace.pubsub "item")
    in
    Pubsub.publish ~to':(Jid.bare jid)
      ~node:"net.openengiadina.xmpp.activitystreams" client (Some item)
end
