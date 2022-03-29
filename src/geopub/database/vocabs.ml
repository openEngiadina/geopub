(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr_io
open Lwt

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Database"

module Log = (val Logs.src_log src : Logs.LOG)

let promise_of_fut fut =
  let p, resolver = Lwt.wait () in
  Fut.await fut (fun v -> Lwt.wakeup resolver v);
  p

let promise_of_fut_or_error fut =
  fut |> promise_of_fut >>= function
  | Ok v -> return v
  | Error e -> fail @@ Jv.Error e

let vocabs = [ "activitystreams2.xml"; "musicontology.xml" ]

let parse_xml s =
  Xmlm.make_input ~strip:true (`String (0, s))
  |> Rdf_xml.xmlm_input_to_seq |> Rdf_xml.parse_to_graph

let fetch_vocab vocab =
  Fetch.url (Jstr.v @@ "vocabs/" ^ vocab)
  |> promise_of_fut_or_error >|= Fetch.Response.as_body
  >>= fun body ->
  promise_of_fut_or_error @@ Fetch.Body.text body
  >|= Jstr.to_string >|= parse_xml
