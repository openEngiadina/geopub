(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt

let view_iri iri =
  El.(
    a
      ~at:At.[ href @@ Route.to_jstr @@ Route.Inspect iri ]
      [ txt' @@ Rdf.Iri.to_string iri ])

let view_blank_node bnode = El.(txt' @@ "_:" ^ Rdf.Blank_node.identifier bnode)
let view_literal literal = El.(txt' @@ Rdf.Literal.canonical literal)

let view_pretty_iri _database iri =
  (* let* label_opt = Database.get_rdfs_label database iri in *)
  (* TODO WIP *)
  let label_opt = None in
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
  Rdf.Triple.Object.map (view_pretty_iri database)
    (fun bnode -> return @@ view_blank_node bnode)
    (fun literal -> return @@ view_literal literal)
    o

let view_subject database =
  Rdf.Triple.Subject.map (view_pretty_iri database) (fun bnode ->
      return @@ view_blank_node bnode)
