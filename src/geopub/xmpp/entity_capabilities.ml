(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(** Enitity capability (XEP-0115) responder *)

(* This is necessary for receiving PubSub notifications *)

open Lwt
open Lwt_react
open Archi_lwt

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Xmpp.Entity_capabilities"

module Log = (val Logs.src_log src : Logs.LOG)

(* XMPP modules *)

module Client = Xmppl_websocket.Client
module Entity_capabilities = Xmppl_entity_capabilities.Make (Client)

type t = unit event

let start _ (connection : Connection.t) =
  (* Initiate Entity Cababilities (XEP-0115) responder *)
  connection |> Connection.client_signal |> S.changes
  |> E.map_s (function
       | Loadable.Loaded client ->
           Entity_capabilities.advertise ~category:"client" ~type':"web"
             ~name:"GeoPub" ~node:"https://codeberg.org/openEngiadina/geopub"
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
           >|= E.map (fun jid_opt ->
                   Log.debug (fun m ->
                       m "Responding to entity capability request from: %a"
                         (Fmt.option Connection.Jid.pp)
                         jid_opt))
           >|= fun e -> E.stamp e ()
       | _ -> return @@ E.never)
  |> E.switch E.never |> return_ok

let stop _ = return_unit

let component =
  Component.using ~start ~stop ~dependencies:[ Connection.component ]
