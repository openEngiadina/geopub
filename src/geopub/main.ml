(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax
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
    ]

let main () =
  (* Setup logging *)
  Logs.set_reporter @@ Logs_browser.console_reporter ();

  Logs.set_level @@ Some Logs.Debug;

  (* Logs.set_level @@ Some Logs.Info; *)

  (* Initialize the application *)
  let () = Log.app (fun m -> m "Initializing GeoPub.") in

  (* Show a loading screen *)
  let body = Document.body G.document in
  El.set_children body @@ Ui.loading "Initializing GeoPub ... ";

  (* Initialize random generator *)
  Random.self_init ();

  (* Start the app *)
  let* system = System.start () system in

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
