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

type t =
  | About
  | Activity of Leaflet.LatLng.t option
  | Map
  | Inspect of Rdf.Iri.t
  | Settings

let parser uri =
  let path =
    String.split_on_char '=' @@ Option.value ~default:"" @@ Rdf.Iri.fragment uri
  in
  match path with
  | [ "about" ] -> About
  | [ "activity" ] -> Activity None
  | [ "map" ] -> Map
  | [ "inspect"; encoded_iri_s ] -> (
      match Uri.decode @@ Jstr.v encoded_iri_s with
      | Ok iri_jstr -> Inspect (Rdf.Iri.of_string @@ Jstr.to_string iri_jstr)
      | Error error ->
          Console.error [ error ];
          Inspect (Rdf.Iri.of_string "urn:something:went:wrong"))
  | [ "settings" ] -> Settings
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
  | Activity None -> Uri.with_uri location ~fragment:(Jstr.v "activity")
  | Activity (Some latlng) ->
      let latlng_s =
        (string_of_float @@ Leaflet.LatLng.lat latlng)
        ^ "/" ^ string_of_float @@ Leaflet.LatLng.lng latlng
      in
      Uri.with_uri location ~fragment:(Jstr.v @@ "activity" ^ "=" ^ latlng_s)
  | Map -> Uri.with_uri location ~fragment:(Jstr.v "map")
  | Inspect iri ->
      let encoded_iri =
        match Uri.encode @@ Jstr.v @@ Rdf.Iri.to_string iri with
        | Ok e -> e
        | Error error ->
            Console.error [ error ];
            Jstr.v "urn:something:went:wrong"
      in

      Uri.with_uri location
        ~fragment:(Jstr.concat [ Jstr.v "inspect="; encoded_iri ])
  | Settings -> Uri.with_uri location ~fragment:(Jstr.v "settings"))
  |> Result.value ~default:location

let to_jstr route = to_uri route |> Uri.to_jstr

let set_route route =
  Window.History.push_state ~uri:(to_uri route) history;
  route

let init () = get_location ()
