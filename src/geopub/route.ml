(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

type t =
  | About
  | User
  | Activity of Leaflet.Latlng.t option
  | Map
  | Query of string
  | Settings

let parser uri =
  let path =
    String.split_on_char '=' @@ Option.value ~default:"" @@ Rdf.Iri.fragment uri
  in
  match path with
  | [ "about" ] -> About
  | [ "user" ] -> User
  | [ "activity" ] -> Activity None
  | [ "map" ] -> Map
  | [ "query"; query_b32 ] -> Query (Base32.decode_exn query_b32)
  | [ "settings" ] -> Settings
  | _ -> About

let to_uri route =
  let location = Window.location G.window in
  (match route with
  | About -> Uri.with_uri location ~fragment:(Jstr.v "about")
  | User -> Uri.with_uri location ~fragment:(Jstr.v "user")
  | Activity None -> Uri.with_uri location ~fragment:(Jstr.v "activity")
  | Activity (Some _latlng) ->
      Uri.with_uri location ~fragment:(Jstr.v @@ "activity")
  | Map -> Uri.with_uri location ~fragment:(Jstr.v "map")
  | Query query ->
      Uri.with_uri location
        ~fragment:
          (Jstr.concat
             [
               Jstr.v "query="; Jstr.v @@ Base32.encode_string ~pad:false query;
             ])
  | Settings -> Uri.with_uri location ~fragment:(Jstr.v "settings"))
  |> Result.value ~default:location

let to_jstr route = to_uri route |> Uri.to_jstr
let href route = At.href @@ to_jstr route
