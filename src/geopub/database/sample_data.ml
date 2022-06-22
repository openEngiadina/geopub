(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr_io
open Lwt
open Lwt.Syntax

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

let sample_data = [ "hello.nt" ]

let fetch file =
  let* s =
    Fetch.url (Jstr.v @@ "sample_data/" ^ file)
    |> promise_of_fut_or_error >|= Fetch.Response.as_body
    >>= fun body ->
    promise_of_fut_or_error @@ Fetch.Body.text body >|= Jstr.to_string
  in
  Rdf.Graph.add_seq (s |> String.to_seq |> Rdf_ntriples.parse) Rdf.Graph.empty
  |> return
