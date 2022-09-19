(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)
(* Configure the ActivityStrems PEP node *)

open Lwt
open Lwt.Syntax
open Lwt_react

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Xmpp"

module Log = (val Logs.src_log src : Logs.LOG)

(* XMPP modules *)

module Client = Xmppl_websocket.Client
module Pubsub = Xmppl_pubsub.Make (Client)
module Jid = Xmppl.Jid

let as_node = "net.openengiadina.xmpp.activitystreams"

(* Enable persistence on the ActivityStreams PEP node *)
let configure_pep_node client =
  let* jid = Client.jid client >|= Jid.bare in

  (* let* () =
   *   Client.iq_get client ~to':jid
   *     Xmlc.Tree.(
   *       make_element
   *         ~attributes:
   *           [
   *             Xmlc.Namespace.default_xmlns_declaration PubSub.Namespace.owner_ns;
   *           ]
   *         ~children:
   *           [
   *             make_element
   *               ~attributes:[ (PubSub.Namespace.owner "node", as_node) ]
   *               (PubSub.Namespace.owner "configure");
   *           ]
   *         (PubSub.Namespace.owner "pubsub"))
   *   >>= fun result ->
   *   Log.debug (fun m -> m "%a" Xmppl.Stanza.Iq.pp_result result)
   * in *)
  let data_ns = "jabber:x:data" in
  let data local = (data_ns, local) in

  (* TODO: Abstract this into a nice Xmppl_data_forms module. What you see here is low-level hacking. *)
  let config =
    Xmlc.Tree.(
      make_element
        ~attributes:
          [
            Xmlc.Namespace.default_xmlns_declaration data_ns;
            (data "type", "submit");
          ]
        ~children:
          [
            make_element
              ~attributes:[ (data "var", "pubsub#max_items") ]
              ~children:
                [ make_element ~children:[ make_data "256" ] (data "value") ]
              (data "field");
            make_element
              ~attributes:[ (data "var", "pubsub#persist_items") ]
              ~children:
                [ make_element ~children:[ make_data "1" ] (data "value") ]
              (data "field");
          ]
        (data "x"))
  in

  Lwt_result.catch
  @@ Client.iq_set client ~to':jid
       Xmlc.Tree.(
         make_element
           ~attributes:
             [
               Xmlc.Namespace.default_xmlns_declaration
                 Pubsub.Namespace.owner_ns;
             ]
           ~children:
             [
               make_element
                 ~attributes:[ (Pubsub.Namespace.owner "node", as_node) ]
                 ~children:[ config ]
                 (Pubsub.Namespace.owner "configure");
             ]
           (Pubsub.Namespace.owner "pubsub"))
  >>= function
  | Ok _ ->
      return
      @@ Log.info (fun m ->
             m "Configured XMPP PEP node %s to persist 256 items." as_node)
  | Error e ->
      return
      @@ Log.warn (fun m ->
             m "Could not configure XMPP PEP node (%s)." (Printexc.to_string e))

(* Component *)

open Archi_lwt

type t = unit event

let start (_ : string -> unit) (connection : Connection.t) =
  (* Initiate Entity Cababilities (XEP-0115) responder *)
  connection |> Connection.client_signal |> S.changes
  |> E.map_s (function
       | Loadable.Loaded client -> configure_pep_node client
       | _ -> return_unit)
  |> return_ok

let stop _ = return_unit

let component =
  Component.using ~start ~stop ~dependencies:[ Connection.component ]
