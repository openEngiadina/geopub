(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax
open Geopub_database

(* Setup logging *)

let src = Logs.Src.create "GeoPub"

module Log = (val Logs.src_log src : Logs.LOG)

let view (model : Model.t) iri =
  let* _description =
    get_description model.database (Rdf.Triple.Subject.of_iri iri)
  in
  Log.debug (fun m -> m "got description");
  return
    El.
      [
        Ui.geopub_menu model;
        div
          ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
          [ h1 [ txt' @@ Rdf.Iri.to_string iri ]; txt' "Inspect" ];
      ]
