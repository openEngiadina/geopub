(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Brr
open Brr_io

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Sample_data"

module Log = (val Logs.src_log src : Logs.LOG)

(* LocalStorage bit that indicates if sample data is already loaded *)

let local_storage_vaue_name = Jstr.v "GeoPub.sample-data-loaded"

let is_loaded () =
  Storage.get_item (Storage.local G.window) local_storage_vaue_name
  |> Option.map (fun s -> not @@ Jstr.is_empty s)
  |> Option.value ~default:false

let set_loaded () =
  Storage.set_item (Storage.local G.window) local_storage_vaue_name
    (Ptime_clock.now () |> Ptime.to_rfc3339 |> Jstr.of_string)

(* Load sample data *)

let promise_of_fut fut =
  let p, resolver = Lwt.wait () in
  Fut.await fut (fun v -> Lwt.wakeup resolver v);
  p

let promise_of_fut_or_error fut =
  fut |> promise_of_fut >>= function
  | Ok v -> return v
  | Error e -> fail @@ Jv.Error e

let sample_data =
  [
    "hello.ttl";
    "activitystreams2.ttl";
    (* "musicontology.ttl"; *)
    "rdfs.ttl";
    (* "owl.ttl"; *)
    "geo.ttl";
    "dublin_core_terms.ttl";
    "foaf.ttl" (* "che.osm.surveillance.nt"; *);
    "vf.ttl";
  ]

let fetch file =
  let* s =
    Fetch.url (Jstr.v @@ "sample_data/" ^ file)
    |> promise_of_fut_or_error >|= Fetch.Response.as_body
    >>= fun body ->
    promise_of_fut_or_error @@ Fetch.Body.text body >|= Jstr.to_string
  in
  s |> String.to_seq |> Rdf_turtle.parse_to_graph |> return

let load_sample_data status db =
  let* () =
    sample_data
    |> Lwt_list.iter_s (fun file ->
           status (Format.sprintf "Loading data from %s ..." file);
           fetch file >>= Database.add_graph db)
  in
  ignore @@ set_loaded ();
  return_ok ()

(* Component that loads sample data and vocabularies *)

open Archi_lwt

let start msg db =
  if is_loaded () then return_ok () else load_sample_data msg db

let stop () = return_unit

let component =
  Component.using ~start ~stop ~dependencies:[ Database.component ]
