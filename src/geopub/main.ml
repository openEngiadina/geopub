(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax
open Lwt_react
open Archi_lwt

(* Setup logging *)

let src = Logs.Src.create "GeoPub"

module Log = (val Logs.src_log src : Logs.LOG)

(* The top-level system *)

let system =
  System.make
    [
      ("ui", Ui.component);
      ("db", Database.component);
      ("xmpp_rdf", Xmpp_rdf.component);
      ("sample_data", Sample_data.component);
    ]

let main () =
  (* Setup logging *)
  Logs.set_reporter @@ Logs_browser.console_reporter ();

  (* Logs.set_level @@ Some Logs.Debug; *)
  Logs.set_level @@ Some Logs.Info;

  (* Initialize the application *)
  let () = Log.app (fun m -> m "Initializing GeoPub.") in

  (* Loading message signal *)
  let loading_msg, set_loading_msg =
    S.create ~eq:String.equal "Loading GeoPub..."
  in

  (* Show a loading screen *)
  let body = Document.body G.document in

  Brr_react.Elr.def_children body (S.map Ui.loading loading_msg) |> S.keep;

  (* Initialize random generator *)
  Random.self_init ();

  (* Start the app *)
  let* system = System.start (fun msg -> set_loading_msg msg) system in

  (match system with
  | Ok _ -> Log.info (fun m -> m "GeoPub started.")
  | Error (`Msg msg) -> Log.err (fun m -> m "Failed to start GeoPub: %s" msg)
  | Error `Cycle_found ->
      Log.err (fun m -> m "Failed to start GeoPub: Cycle_found"));

  return_unit

let () =
  (Lwt.async_exception_hook :=
     fun exn ->
       Log.err (fun m ->
           m "unexpected async exception: %s" @@ Printexc.to_string exn));
  Lwt.on_any (main ()) ignore (function
    | Jv.Error error ->
        Log.err (fun m ->
            m "unexpected JavaScript exception: %s"
            @@ Jstr.to_string @@ Jv.Error.message error)
    | exn ->
        Log.err (fun m ->
            m "unexpected exception: %s" @@ Printexc.to_string exn))
