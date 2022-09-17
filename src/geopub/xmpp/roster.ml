(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* XMPP Roster *)

open Lwt
open Lwt_react

(* XMPP modules *)

module Client = Xmppl_websocket.Client
include Xmppl_roster.Make (Client)

(* Component *)

open Archi_lwt

type t = roster signal

let start _ (connection : Connection.t) =
  connection |> Connection.client_signal |> fun s ->
  S.bind_s s (function
    | Loadable.Loaded client -> roster client
    | _ -> return @@ S.const Connection.Jid.Map.empty)
  >|= Result.ok

let stop _ = return_unit

let component =
  Component.using ~start ~stop ~dependencies:[ Connection.component ]
