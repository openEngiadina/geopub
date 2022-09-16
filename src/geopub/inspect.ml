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
                  dd [ ul ~at:[ UIKit.list; UIKit.Margin.left ] objects_lis ];
                ])

(* Backlinks *)

let view_backlinks database backlinks =
  backlinks
  |> Lwt_list.map_s (fun (s, p) ->
         let* subject_el = Ui_rdf.term database s in
         let* predicate_el = Ui_rdf.term database p in

         return
         @@ El.
              [
                dt [ predicate_el ]; dd ~at:[ UIKit.Margin.left ] [ subject_el ];
              ])
  >|= List.concat
  >|= fun els -> El.(dl ~at:[ UIKit.description_list ] els)

let backlinks database subject =
  let query =
    Database.Datalog.(
      Atom.make "triple"
        Term.
          [
            make_variable "s";
            make_variable "p";
            make_constant @@ Constant.Rdf subject;
          ])
  in

  Database.query database query
  >>= S.map_s (fun (_tx, set) ->
          Database.Datalog.Tuple.Set.to_seq set
          |> Seq.filter_map (function
               | Database.Datalog.Constant.[ Rdf s; Rdf p; _ ] -> Some (s, p)
               | _ -> None)
          |> Lwt_seq.of_seq |> Lwt_seq.to_list)

(* RDFS inferred types *)

let rdfs_types database subject =
  let query =
    Database.Datalog.(
      Atom.make "triple-rhodf"
        Term.
          [
            make_constant @@ Constant.Rdf subject;
            make_constant
            @@ Constant.Rdf (Rdf.Term.of_iri @@ Rdf.Namespace.rdf "type");
            make_variable "o";
          ])
  in
  Database.query database query
  >>= S.map_s (fun (_tx, set) ->
          Database.Datalog.Tuple.Set.to_seq set
          |> Seq.filter_map (function
               | Database.Datalog.Constant.[ _; _; Rdf o ] -> Some o
               | _ -> None)
          |> Seq.filter (fun t ->
                 not @@ Option.is_some @@ Rdf.Term.to_blank_node t)
          |> Lwt_seq.of_seq |> Lwt_seq.to_list)

let view_rdfs_types database types =
  types
  |> Lwt_list.map_s (fun t ->
         let* type_el = Ui_rdf.term database t in
         return @@ El.(li [ type_el ]))
  >|= fun els -> El.(ul ~at:[ UIKit.list ] els)

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

  let subject_term =
    S.map
      (fun description ->
        Rdf.Description.subject description |> Rdf.Triple.Subject.to_term)
      description
  in

  let* backlinks = S.bind_s subject_term (backlinks model.database) in
  let* rdfs_types = S.bind_s subject_term (rdfs_types model.database) in

  S.l3_s
    (fun description backlinks rdfs_types ->
      let* ddl = description_list_of_description model.database description in
      let* backlinks_el = view_backlinks model.database backlinks in
      let* rdf_types_el = view_rdfs_types model.database rdfs_types in
      let* title_el = title_of_description model.database description in
      return
      @@ El.
           [
             div
               ~at:[ UIKit.container; UIKit.margin ]
               [
                 article
                   ~at:[ UIKit.article; UIKit.margin ]
                   [
                     title_el;
                     submenu model.xmpp iri;
                     p
                       ~at:[ UIKit.Article.meta; UIKit.Text.break ]
                       [
                         txt'
                         @@ Rdf.Triple.Subject.map Rdf.Iri.to_string
                              Rdf.Blank_node.identifier
                              (Rdf.Description.subject description);
                       ];
                     ul
                       ~at:[ UIKit.Accordion.multiple ]
                       [
                         li ~at:[ UIKit.open' ]
                           [
                             a
                               ~at:
                                 [
                                   UIKit.Accordion.title; At.href @@ Jstr.v "#";
                                 ]
                               [ txt' "Predicates" ];
                             div ~at:[ UIKit.Accordion.content ] ddl;
                           ];
                         li ~at:[ UIKit.open' ]
                           [
                             a ~at:[ UIKit.Accordion.title ]
                               [ txt' "Backlinks" ];
                             div
                               ~at:[ UIKit.Accordion.content ]
                               [ backlinks_el ];
                           ];
                         li ~at:[]
                           [
                             a ~at:[ UIKit.Accordion.title ]
                               [ txt' "Inferred types" ];
                             div
                               ~at:[ UIKit.Accordion.content ]
                               [ rdf_types_el ];
                           ];
                       ];
                   ];
               ];
           ])
    description backlinks rdfs_types
