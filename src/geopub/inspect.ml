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

let title_decoder =
  Rdf.Decoder.(
    choice
      [
        functional_property
          (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdfs "label")
          any_literal;
      ]
    >>| Rdf.Literal.canonical)

let view_header graph iri =
  let subject = Rdf.Triple.Subject.of_iri iri in
  let subject_title =
    match Rdf.Decoder.decode subject title_decoder graph with
    | Ok title -> title
    | Error _ -> Rdf.Iri.to_string iri
  in
  El.(
    header
      [
        h1 [ txt' subject_title ];
        p
          ~at:At.[ class' @@ Jstr.v "iri"; class' @@ Jstr.v "meta" ]
          [ txt' @@ Rdf.Iri.to_string iri ];
      ])

let view (model : Model.t) iri =
  let* graph = inspect_graph model.database (Rdf.Triple.Subject.of_iri iri) in
  Log.debug (fun m -> m "Inspect graph: %a" Rdf.Graph.pp graph);
  return
    El.
      [
        Ui.geopub_menu model;
        div
          ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
          [ view_header graph iri ];
      ]
