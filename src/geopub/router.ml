(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt_react
open Archi_lwt

(* Setup logging *)

let src = Logs.Src.create "GeoPub.Router"

module Log = (val Logs.src_log src : Logs.LOG)

let history = Window.history G.window

type t = Route.t signal

let get_location () =
  Window.location G.window |> Uri.to_jstr |> Jstr.to_string |> Rdf.Iri.of_string
  |> Route.parser

let start () =
  let s, push_state = S.create @@ get_location () in
  Ev.listen Window.History.Ev.popstate
    (fun _ ->
      get_location ()
      |> (fun route ->
           Log.debug (fun m ->
               m "Route updated to: %s" @@ Jstr.to_string @@ Route.to_jstr route);
           route)
      |> push_state)
    (Window.as_target G.window);
  s |> return_ok

let stop _ = return_unit
let component = Component.make ~start ~stop
let current t = S.value t

let set_route _t route =
  Window.History.push_state ~uri:(Route.to_uri route) history
