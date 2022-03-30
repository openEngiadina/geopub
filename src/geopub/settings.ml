(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Settings"

module Log = (val Logs.src_log src : Logs.LOG)
open Geopub_database

let database_settings ~update (model : Model.t) =
  let* triple_count = Database.triple_count model.database in
  (* let triple_count = model.counter in *)
  return
    El.(
      div
        [
          h2 [ txt' "Database" ];
          p [ txt' ("Triple Count: " ^ string_of_int triple_count) ];
          p
            [
              Ui.on_el Ev.click (fun ev ->
                  (* Disable button and set text *)
                  let button = Ev.(target_to_jv @@ target ev) in
                  Jv.set button "disabled" Jv.true';
                  Jv.set button "textContent"
                  @@ Jv.of_string
                       "Resetting and reloading default vocabularies...";
                  update (fun (model : Model.t) ->
                      let* database = Database.reset model.database in
                      return { model with database }))
              @@ button [ txt' "Reset Database" ];
            ];
        ])

let xmpp_settings (model : Model.t) =
  El.(
    div
      [
        h2 [ txt' "XMPP" ];
        (match model.xmpp with
        | Loadable.Idle -> txt' "Idle"
        | Loadable.Loading -> txt' "Loading"
        | Loadable.Loaded _ -> txt' "Loaded"
        | Loadable.Error _ -> txt' "Error");
      ])

let view ~update (model : Model.t) =
  let* database_settings = database_settings ~update model in
  return
    El.
      [
        Ui.geopub_menu model;
        div
          ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
          [ h1 [ txt' "Settings" ]; xmpp_settings model; database_settings ];
      ]
