(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax

(* Setup logging *)

let src = Logs.Src.create "GeoPub"

module Log = (val Logs.src_log src : Logs.LOG)
module Database = Geopub_database

let title_decoder =
  Rdf.Decoder.(
    choice
      [
        functional_property
          (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdfs "label")
          any_literal;
      ]
    >>| Rdf.Literal.canonical)

let view_header description =
  let subject_title =
    match
      Rdf.Description.functional_property
        (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdfs "label")
        description
    with
    | Some object' ->
        Rdf.Triple.Object.map object' Rdf.Iri.to_string
          Rdf.Blank_node.identifier Rdf.Literal.canonical
    | None ->
        Rdf.Triple.Subject.map
          (Rdf.Description.subject description)
          Rdf.Iri.to_string Rdf.Blank_node.identifier
  in
  El.(
    header
      [
        h1 [ txt' subject_title ];
        p
          ~at:At.[ class' @@ Jstr.v "iri"; class' @@ Jstr.v "meta" ]
          [
            txt'
            @@ Rdf.Triple.Subject.map
                 (Rdf.Description.subject description)
                 Rdf.Iri.to_string Rdf.Blank_node.identifier;
          ];
      ])

let view_description_statements database description =
  Rdf.Description.to_nested_seq description
  |> List.of_seq
  |> Lwt_list.map_s
       El.(
         fun (predicate, objects_seq) ->
           let* predicate_el = Ui_rdf.view_predicate database predicate in
           let* objects_lis =
             objects_seq |> List.of_seq
             |> Lwt_list.map_s (fun o ->
                    let* object_el = Ui_rdf.view_object database o in
                    return @@ li [ object_el ])
           in
           return
           @@ dl
                ~at:At.[ class' @@ Jstr.v "description" ]
                [
                  dt [ predicate_el ];
                  dd [ ul ~at:At.[ class' @@ Jstr.v "objects" ] objects_lis ];
                ])

let view (model : Model.t) iri =
  let* description = Database.get_description model.database iri in
  let* statements = view_description_statements model.database description in
  return
    El.
      [
        Ui.geopub_menu model;
        div
          ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
          ([ view_header description ] @ statements);
      ]
