(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Lwt_react

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Xmpp.Connection"

module Log = (val Logs.src_log src : Logs.LOG)

(* Xmpp Client *)

module Jid = Xmppl.Jid
module Stanza = Xmppl.Stanza
module Client = Xmppl_websocket.Client

(* Component *)

open Archi_lwt

type loadable_client = (Client.t, exn) Loadable.t
type t = loadable_client signal * (?step:React.step -> loadable_client -> unit)

let start (_msg : string -> unit) : (t, [ `Msg of string ]) Result.t Lwt.t =
  S.create ~eq:( == ) Loadable.Idle |> return_ok

let stop (state, set_state) =
  match S.value state with
  | Loadable.Loaded client ->
      let* () = Client.disconnect client in
      return @@ set_state ?step:None Loadable.Idle
  | _ -> return_unit

let component = Component.make ~start ~stop

(* Component API *)

let client_signal (state, _) = state

let client (state, _) =
  Loadable.to_result
    ~idle:(fun () -> Error (Failure "Not connected"))
    ~loading:(fun () -> Error (Failure "Loading"))
    (S.value state)

(* Login methods *)
let login (_, set_state) ?(options = Xmppl_websocket.default_options) ~password
    jid =
  set_state ?step:None Loadable.Loading;
  let* client =
    Client.create options ~credentials:(`JidPassword (jid, password))
  in

  Client.connect client |> Lwt_result.catch
  >|= Result.map (fun () -> client)
  >|= Loadable.of_result >|= set_state ?step:None

let login_anonymous_demo (_, set_state) =
  set_state ?step:None Loadable.Loading;

  let* client =
    Client.create
      { ws_endpoint = Some "wss://openengiadina.net/xmpp-websocket" }
      ~credentials:(`Anonymous "demo.openengiadina.net")
  in

  Client.connect client |> Lwt_result.catch
  >|= Result.map (fun () -> client)
  >|= Loadable.of_result >|= set_state ?step:None
