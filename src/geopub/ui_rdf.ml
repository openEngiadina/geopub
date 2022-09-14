(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt
open Lwt.Syntax

(* RDF Terms *)

let blank_node bnode = El.(txt' @@ "_:" ^ Rdf.Blank_node.identifier bnode)
let literal literal = El.(div [ txt' @@ Rdf.Literal.canonical literal ])

let iri_plain iri =
  match iri with
  | iri when Rdf.Iri.equal iri (Rdf.Namespace.rdf "type") -> El.txt' "type"
  | _ ->
      El.(
        a
          ~at:[ Route.href @@ Route.Inspect iri ]
          [ txt' @@ Rdf.Iri.to_string iri ])

let iri database iri =
  let* label_opt =
    Database.get_functional_property database
      (Rdf.Triple.Subject.of_iri iri)
      (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdfs "label")
    >|= fun o -> Option.bind o Rdf.Triple.Object.to_literal
  in
  match label_opt with
  | Some l ->
      return @@ El.(a ~at:[ Route.href @@ Route.Inspect iri ] [ literal l ])
  | None -> return @@ iri_plain iri

(* Triple *)

let subject database =
  Rdf.Triple.Subject.map (iri database) (fun bnode ->
      return @@ blank_node bnode)

let predicate database p = iri database @@ Rdf.Triple.Predicate.to_iri p

let object' database o =
  Rdf.Triple.Object.map (iri database)
    (fun b -> return @@ blank_node b)
    (fun l -> return @@ literal l)
    o
