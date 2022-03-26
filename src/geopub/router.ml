(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt_react

let history = Window.history G.window

type route = About | Login | Map | Inspect of Rdf.Iri.t

let parser uri =
  let fragment = Jstr.to_string @@ Uri.fragment uri in
  match fragment with "about" -> About | "login" -> Login | _ -> About

let update =
  let e, push_state = E.create () in
  Ev.listen Window.History.Ev.popstate
    (fun _ ->
      let location = Window.location G.window in
      push_state @@ parser location)
    (Window.as_target G.window);
  e

let to_uri route =
  let location = Window.location G.window in
  (match route with
  | About -> Uri.with_uri location ~fragment:(Jstr.v "about")
  | Login -> Uri.with_uri location ~fragment:(Jstr.v "login")
  | Map -> Uri.with_uri location ~fragment:(Jstr.v "map")
  | Inspect iri ->
      Uri.with_uri location ~fragment:(Jstr.v "inspect")
        ~query:(Jstr.v ("iri=" ^ Rdf.Iri.to_string iri)))
  |> Result.value ~default:location

let set_route route =
  Window.History.push_state ~uri:(to_uri route) history;
  route

let init () = set_route About
