(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Brr_react
open Lwt
open Lwt.Syntax
open Lwt_react

let title_of_description database description =
  let* title_s =
    Ui_rdf.subject database @@ Rdf.Description.subject description
  in
  return @@ El.(h1 ~at:[ UIKit.Article.title; UIKit.Text.truncate ] [ title_s ])

let view_header description =
  let subject_title =
    match
      Rdf.Description.functional_property
        (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdfs "label")
        description
    with
    | Some object' ->
        Rdf.Triple.Object.map Rdf.Iri.to_string Rdf.Blank_node.identifier
          Rdf.Literal.canonical object'
    | None ->
        Rdf.Triple.Subject.map Rdf.Iri.to_string Rdf.Blank_node.identifier
          (Rdf.Description.subject description)
  in
  El.(
    header
      [
        h1 [ txt' subject_title ];
        p
          ~at:At.[ class' @@ Jstr.v "iri"; class' @@ Jstr.v "meta" ]
          [
            txt'
            @@ Rdf.Triple.Subject.map Rdf.Iri.to_string
                 Rdf.Blank_node.identifier
                 (Rdf.Description.subject description);
          ];
      ])

let description_list_of_description database description =
  Rdf.Description.to_nested_seq description
  |> List.of_seq
  |> Lwt_list.map_s
       El.(
         fun (predicate, objects_seq) ->
           let* predicate_el = Ui_rdf.predicate database predicate in
           let* objects_lis =
             objects_seq |> List.of_seq
             |> Lwt_list.map_s (fun o ->
                    let* object_el = Ui_rdf.object' database o in
                    return @@ li [ object_el ])
           in
           return
           @@ dl ~at:[ UIKit.description_list ]
                [
                  dt [ predicate_el ];
                  dd [ ul ~at:At.[ class' @@ Jstr.v "objects" ] objects_lis ];
                ])

(* let view_backlinks database description =
 *   let subject_term =
 *     Rdf.Triple.Subject.to_term @@ Rdf.Description.subject description
 *   in
 *   let* subject_id = Database.Store.Dictionary.lookup database subject_term in
 *   match subject_id with
 *   | Some subject_id ->
 *       let query =
 *         Database.Datalog.(
 *           Atom.make "triple"
 *             Term.
 *               [
 *                 make_variable "s";
 *                 make_variable "p";
 *                 make_constant @@ Constant.Rdf subject_id;
 *               ])
 *       in
 *       let backlink_triples = Database.query_triple database query in
 *       let* backlink_dtdds =
 *         backlink_triples |> Lwt_seq.to_list
 *         >>= Lwt_list.map_s (fun (triple : Rdf.Triple.t) ->
 *                 let* predicate_el =
 *                   Ui_rdf.view_predicate database triple.predicate
 *                 in
 *                 let* subject_el = Ui_rdf.view_subject database triple.subject in
 *                 return El.[ dt [ predicate_el ]; dd [ subject_el ] ])
 *         >|= List.concat
 *       in
 *       return El.[ h2 [ txt' "Backlinks" ]; dl backlink_dtdds ]
 *   | None -> return_nil
 * 
 * let view_rhodf_types database description =
 *   let subject_term =
 *     Rdf.Triple.Subject.to_term @@ Rdf.Description.subject description
 *   in
 *   let* subject_id = Database.Store.Dictionary.lookup database subject_term in
 *   let type_id =
 *     Database.Store.Dictionary.constant_lookup @@ Rdf.Term.of_iri
 *     @@ Rdf.Namespace.rdf "type"
 *     |> Option.value ~default:(-99)
 *   in
 *   match subject_id with
 *   | Some subject_id ->
 *       let query =
 *         Database.Datalog.(
 *           Atom.make "triple-rhodf"
 *             Term.
 *               [
 *                 make_constant @@ Constant.Rdf subject_id;
 *                 make_constant @@ Constant.Rdf type_id;
 *                 make_variable "o";
 *               ])
 *       in
 *       let* type_triples =
 *         Database.query_triple database query |> Lwt_seq.to_list
 *       in
 *       let* type_lis =
 *         Lwt_list.map_s
 *           (fun (triple : Rdf.Triple.t) ->
 *             let* object_el = Ui_rdf.view_object database triple.object' in
 *             return El.[ li [ object_el ] ])
 *           type_triples
 *         >|= List.concat
 *       in
 *       return El.[ h2 [ txt' "Inferred types" ]; ul type_lis ]
 *   | None -> return_nil *)

let submenu xmpp iri =
  El.(
    ul
      ~at:At.[ class' @@ Jstr.v "uk-subnav" ]
      [
        li ~at:[ UIKit.Width.expand ] [];
        li
          [
            Evf.on_el Ev.click (fun _ ->
                ignore @@ Activity.Publish.like xmpp iri)
            @@ a ~at:[ UIKit.Icon.star ] [];
          ];
      ])

let view (model : Model.t) iri =
  let* description =
    Database.description model.database @@ Rdf.Term.of_iri iri
  in

  (* let* backlinks = view_backlinks model.database description in *)
  (* let* rhodf_types = view_rhodf_types model.database description in *)
  S.map_s
    (fun description ->
      let* ddl = description_list_of_description model.database description in
      let* title_el = title_of_description model.database description in
      return
      @@ El.
           [
             div
               ~at:[ UIKit.container; UIKit.margin ]
               [
                 article
                   ~at:[ UIKit.article; UIKit.margin ]
                   ([
                      title_el;
                      submenu model.xmpp iri;
                      p ~at:[ UIKit.Article.meta ]
                        [
                          txt'
                          @@ Rdf.Triple.Subject.map Rdf.Iri.to_string
                               Rdf.Blank_node.identifier
                               (Rdf.Description.subject description);
                        ];
                    ]
                   @ ddl);
               ];
           ])
    description
