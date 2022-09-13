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
  | Inspect of Rdf.Iri.t
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
  | [ "query"; query ] -> Query query
  | [ "inspect"; encoded_iri_s ] -> (
      match Uri.decode @@ Jstr.v encoded_iri_s with
      | Ok iri_jstr -> Inspect (Rdf.Iri.of_string @@ Jstr.to_string iri_jstr)
      | Error error ->
          Console.error [ error ];
          Inspect (Rdf.Iri.of_string "urn:something:went:wrong"))
  | [ "settings" ] -> Settings
  | _ -> About

let to_uri route =
  let location = Window.location G.window in
  (match route with
  | About -> Uri.with_uri location ~fragment:(Jstr.v "about")
  | User -> Uri.with_uri location ~fragment:(Jstr.v "user")
  | Activity None -> Uri.with_uri location ~fragment:(Jstr.v "activity")
  | Activity (Some latlng) ->
      let latlng_s =
        (string_of_float @@ Leaflet.Latlng.lat latlng)
        ^ "/" ^ string_of_float @@ Leaflet.Latlng.lng latlng
      in
      Uri.with_uri location ~fragment:(Jstr.v @@ "activity" ^ "=" ^ latlng_s)
  | Map -> Uri.with_uri location ~fragment:(Jstr.v "map")
  | Query query ->
      Uri.with_uri location
        ~fragment:(Jstr.concat [ Jstr.v "query="; Jstr.v query ])
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
let href route = At.href @@ to_jstr route
