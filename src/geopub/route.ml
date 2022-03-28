(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt_react

(* Setup logging *)

let src = Logs.Src.create "GeoPub.Router"

module Log = (val Logs.src_log src : Logs.LOG)

let history = Window.history G.window

type t = About | Login | Map | Inspect of Rdf.Iri.t

(* type t =
 *   | About
 *   | Login
 *   | Map
 *   | Activity
 *   | Roster
 *   | RosterItem of Xmpp.Jid.t
 *   | AddContact *)

let parser uri =
  let path =
    String.split_on_char '/' @@ Option.value ~default:"" @@ Rdf.Iri.fragment uri
  in
  match path with
  | [ "about" ] -> About
  | [ "login" ] -> Login
  | [ "map" ] -> Map
  | [ "inspect"; iri_s ] -> Inspect (Rdf.Iri.of_string iri_s)
  | _ -> About

let get_location () =
  Window.location G.window |> Uri.to_jstr |> Jstr.to_string |> Rdf.Iri.of_string
  |> parser

let update =
  let e, push_state = E.create () in
  Ev.listen Window.History.Ev.popstate
    (fun _ -> get_location () |> push_state)
    (Window.as_target G.window);
  e

let to_uri route =
  let location = Window.location G.window in
  (match route with
  | About -> Uri.with_uri location ~fragment:(Jstr.v "about")
  | Login -> Uri.with_uri location ~fragment:(Jstr.v "login")
  | Map -> Uri.with_uri location ~fragment:(Jstr.v "map")
  | Inspect iri ->
      Uri.with_uri location
        ~fragment:(Jstr.v @@ "inspect/" ^ Rdf.Iri.to_string iri))
  |> Result.value ~default:location

let set_route route =
  Window.History.push_state ~uri:(to_uri route) history;
  route

let init () = get_location ()
