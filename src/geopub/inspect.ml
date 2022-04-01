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

let view_iri iri =
  El.(
    a
      ~at:At.[ href @@ Route.to_jstr @@ Route.Inspect iri ]
      [ txt' @@ Rdf.Iri.to_string iri ])

let view_blank_node bnode = El.(txt' @@ "_:" ^ Rdf.Blank_node.identifier bnode)
let view_literal literal = El.(txt' @@ Rdf.Literal.canonical literal)

let view_pretty_iri database iri =
  let* label_opt = Database.get_rdfs_label database iri in
  match label_opt with
  | Some literal ->
      return
      @@ El.(
           a
             ~at:At.[ href @@ Route.to_jstr @@ Route.Inspect iri ]
             [ view_literal literal ])
  | None ->
      if iri = Rdf.Namespace.rdf "type" then return @@ El.txt' "type"
      else return @@ view_iri iri

let view_predicate database p =
  view_pretty_iri database @@ Rdf.Triple.Predicate.to_iri p

let view_object database o =
  Rdf.Triple.Object.map o (view_pretty_iri database)
    (fun bnode -> return @@ view_blank_node bnode)
    (fun literal -> return @@ view_literal literal)

let view_subject database s =
  Rdf.Triple.Subject.map s (view_pretty_iri database) (fun bnode ->
      return @@ view_blank_node bnode)

let view_description_statements database description =
  Rdf.Description.to_nested_seq description
  |> List.of_seq
  |> Lwt_list.map_s
       El.(
         fun (predicate, objects_seq) ->
           let* predicate_el = view_predicate database predicate in
           let* objects_lis =
             objects_seq |> List.of_seq
             |> Lwt_list.map_s (fun o ->
                    let* object_el = view_object database o in
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
