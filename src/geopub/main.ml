(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax
open Lwt_react

(* Setup logging *)

let src = Logs.Src.create "GeoPub"

module Log = (val Logs.src_log src : Logs.LOG)

(* GeoPub components *)

open Geopub_database

let view (model : Model.t) =
  match model.route with
  | Route.About -> return [ Ui.geopub_menu model; Ui.about ]
  | _ -> Ui.placeholder model

let main () =
  (* Setup logging *)
  Logs.set_reporter @@ Logs_browser.console_reporter ();
  Logs.set_level @@ Some Logs.Debug;

  (* Logs.set_level @@ Some Logs.Info; *)

  (* Initialize the application *)
  let () = Log.app (fun m -> m "Initializing GeoPub.") in

  (* Initialize random generator *)
  Random.self_init ();

  (* Initialize the database *)
  let* database = Database.init () in

  let route = Route.init () in

  let route_updater =
    Route.update |> E.map (fun route (model : Model.t) -> { model with route })
  in

  (* Initialize model *)
  let model_s = S.accum route_updater { database; route } in

  (* Set UI *)
  let body = Document.body G.document in

  let* () =
    model_s
    |> S.map_s (fun model -> view model >|= El.set_children body)
    >|= S.keep
  in

  return_unit

let () =
  Lwt.on_any (main ()) ignore (fun exn ->
      Log.err (fun m -> m "unexpected exception: %s" @@ Printexc.to_string exn))
