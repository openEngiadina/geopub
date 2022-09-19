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

type t = Route.t signal * (Route.t -> unit)

let get_location () =
  Window.location G.window |> Uri.to_jstr |> Jstr.to_string |> Rdf.Iri.of_string
  |> Route.parser

let start _ =
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
  (s, fun route -> push_state route) |> return_ok

let stop _ = return_unit
let component = Component.make ~start ~stop
let route (s, _) = s
let current (s, _) = S.value s

let set_route (_, push_state) route =
  push_state route;
  Window.History.push_state ~uri:(Route.to_uri route) history
