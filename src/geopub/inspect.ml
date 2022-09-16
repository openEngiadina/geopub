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

  let* backlinks =
    S.bind_s description (fun description ->
        let subject =
          Rdf.Description.subject description |> Rdf.Triple.Subject.to_term
        in
        backlinks model.database subject)
  in
  (* let* rhodf_types = view_rhodf_types model.database description in *)
  S.l2_s
    (fun description backlinks ->
      let* ddl = description_list_of_description model.database description in
      let* backlinks_el = view_backlinks model.database backlinks in
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
                      p
                        ~at:[ UIKit.Article.meta; UIKit.Text.break ]
                        [
                          txt'
                          @@ Rdf.Triple.Subject.map Rdf.Iri.to_string
                               Rdf.Blank_node.identifier
                               (Rdf.Description.subject description);
                        ];
                    ]
                   @ (h3 [ txt' "Predicates" ] :: ddl)
                   @ [ h3 [ txt' "Backlinks" ]; backlinks_el ]);
               ];
           ])
    description backlinks
