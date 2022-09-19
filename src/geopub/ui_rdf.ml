(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Brr_react
open Lwt
open Lwt.Syntax

(* RDF Terms *)

let blank_node bnode = El.(txt' @@ "_:" ^ Rdf.Blank_node.identifier bnode)

let literal ?(with_lang = true) literal =
  match Rdf.Literal.language literal with
  | None -> El.(div [ txt' @@ Rdf.Literal.canonical literal ])
  | Some language ->
      El.(
        div
          ~at:At.(add_if with_lang (title @@ Jstr.v language) [])
          [ txt' @@ Rdf.Literal.canonical literal ])

let iri_plain inspector iri =
  match iri with
  | iri when Rdf.Iri.equal iri (Rdf.Namespace.rdf "type") ->
      Evf.on_el Ev.click (fun _ -> inspector iri)
      @@ El.a ~at:[] [ El.txt' "type" ]
  | _ ->
      El.(
        Evf.on_el Ev.click (fun _ -> inspector iri)
        @@ a ~at:[] [ txt' @@ Rdf.Iri.to_string iri ])

let get_label database iri =
  Database.get_functional_property database
    (Rdf.Triple.Subject.of_iri iri)
    (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdfs "label")
  >|= fun o -> Option.bind o Rdf.Triple.Object.to_literal

let get_type database iri =
  Database.get_functional_property database
    (Rdf.Triple.Subject.of_iri iri)
    (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdf "type")
  >|= fun o -> Option.bind o Rdf.Triple.Object.to_iri

let get_dc_title database iri =
  Database.get_functional_property database
    (Rdf.Triple.Subject.of_iri iri)
    (Rdf.Triple.Predicate.of_iri @@ Namespace.dc "title")
  >|= fun o -> Option.bind o Rdf.Triple.Object.to_literal

let get_type_label database iri =
  let* type' = get_type database iri in
  match type' with Some iri -> get_label database iri | None -> return_none

let iri inspector ?href database iri =
  let* label_opt =
    get_label database iri >>= function
    | None -> (
        get_dc_title database iri >>= function
        | None -> get_type_label database iri
        | Some l -> return_some l)
    | Some l -> return_some l
  in

  match label_opt with
  | Some l ->
      return
      @@ El.(
           Evf.on_el Ev.click (fun _ ->
               match href with
               | Some href -> inspector href
               | None -> inspector iri)
           @@ a
                ~at:
                  (match href with
                  | Some href ->
                      [ At.title @@ Jstr.v @@ Rdf.Iri.to_string href ]
                  | None -> [ At.title @@ Jstr.v @@ Rdf.Iri.to_string iri ])
                [ literal ~with_lang:false l ])
  | None -> return @@ iri_plain inspector iri

let term ?href inspector database =
  Rdf.Term.map
    (iri ?href inspector database)
    (fun b -> return @@ blank_node b)
    (fun l -> return @@ literal l)

(* Triple *)

let subject inspector database =
  Rdf.Triple.Subject.map (iri inspector database) (fun bnode ->
      return @@ blank_node bnode)

let predicate inspector database p =
  iri inspector database @@ Rdf.Triple.Predicate.to_iri p

let object' ?href inspector database o =
  Rdf.Triple.Object.to_term o |> term inspector ?href database

let object_option ?href inspector database = function
  | Some o -> object' ?href inspector database o
  | None -> return @@ El.txt' ""
