(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* Parse RDF from XMPP stanzas and add to Datbase *)

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

let start (_ : string -> unit) database connection =
  connection |> Xmpp.Connection.client_signal |> S.changes
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
  |> E.switch E.never |> return_ok

let stop _ = return_unit

let component =
  Component.using ~start ~stop
    ~dependencies:[ Database.component; Xmpp.Connection.component ]
