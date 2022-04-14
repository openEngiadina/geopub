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

let view_backlinks database description =
  let subject_term =
    Rdf.Triple.Subject.to_term @@ Rdf.Description.subject description
  in
  let* subject_id = Database.Store.Dictionary.lookup database subject_term in
  match subject_id with
  | Some subject_id ->
      let query =
        Database.Datalog.(
          Atom.make "rdf"
            Term.
              [ make_variable "s"; make_variable "p"; make_constant subject_id ])
      in
      let* backlink_triples =
        Database.query database query
        >|= Database.Datalog.Tuple.Set.to_seq >|= List.of_seq
        >>= Lwt_list.filter_map_p (Database.Store.Triples.deref database)
      in
      let* backlink_dtdds =
        Lwt_list.map_s
          (fun (triple : Rdf.Triple.t) ->
            let* predicate_el =
              Ui_rdf.view_predicate database triple.predicate
            in
            let* subject_el = Ui_rdf.view_subject database triple.subject in
            return El.[ dt [ predicate_el ]; dd [ subject_el ] ])
          backlink_triples
        >|= List.concat
      in
      return El.[ h2 [ txt' "Backlinks" ]; dl backlink_dtdds ]
  | None -> return_nil

let view (model : Model.t) iri =
  let* description = Database.get_description model.database iri in
  let* statements = view_description_statements model.database description in
  let* backlinks = view_backlinks model.database description in
  return
    El.
      [
        Ui.geopub_menu model;
        div
          ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
          ([ view_header description ] @ statements @ backlinks);
      ]
