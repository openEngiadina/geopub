(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax
module Database = Geopub_database

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