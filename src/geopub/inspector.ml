(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Lwt_react

(* History to navigate the inspector *)

module History = struct
  type t = {
    stack : Rdf.Iri.t list signal;
    reset : Rdf.Iri.t -> unit;
    push : Rdf.Iri.t -> unit;
    take : int -> unit;
  }

  let make () =
    let e, do' = E.create () in
    let stack = S.accum ~eq:(List.equal Rdf.Iri.equal) e [] in
    {
      stack;
      reset = (fun iri -> do' (fun _ -> [ iri ]));
      push = (fun iri -> do' (fun l -> iri :: l));
      take =
        (fun c ->
          do' (fun l ->
              List.rev l |> List.to_seq |> Seq.take c |> List.of_seq |> List.rev));
    }

  let current t = S.map (function hd :: _tail -> Some hd | [] -> None) t.stack
end

(* Offcanvas *)

open Brr

let offcanvas_bar () =
  El.(
    div
      ~at:
        At.
          [
            id @@ Jstr.v "creator";
            v (Jstr.v "uk-offcanvas") (Jstr.v "flip: true; mode: none");
          ]
      [])

module Inspect = struct
  let title_of_description set_iri database description =
    let* title_s =
      Ui_rdf.subject set_iri database @@ Rdf.Description.subject description
    in
    return
    @@ El.(h1 ~at:[ UIKit.Article.title; UIKit.Text.truncate ] [ title_s ])

  let description_list_of_description set_iri database description =
    Rdf.Description.to_nested_seq description
    |> List.of_seq
    |> Lwt_list.map_s
         El.(
           fun (predicate, objects_seq) ->
             let* predicate_el = Ui_rdf.predicate set_iri database predicate in
             let* objects_lis =
               objects_seq |> List.of_seq
               |> Lwt_list.map_s (fun o ->
                      let* object_el = Ui_rdf.object' set_iri database o in
                      return @@ li [ object_el ])
             in
             return
             @@ dl ~at:[ UIKit.description_list ]
                  [
                    dt [ predicate_el ];
                    dd [ ul ~at:[ UIKit.list; UIKit.Margin.left ] objects_lis ];
                  ])

  (* Backlinks *)

  let view_backlinks set_iri database backlinks =
    backlinks
    |> Lwt_list.map_s (fun (s, p) ->
           let* subject_el = Ui_rdf.term set_iri database s in
           let* predicate_el = Ui_rdf.term set_iri database p in

           return
           @@ El.
                [
                  dt [ predicate_el ];
                  dd ~at:[ UIKit.Margin.left ] [ subject_el ];
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

  let view_rdfs_types set_iri database types =
    types
    |> Lwt_list.map_s (fun t ->
           let* type_el = Ui_rdf.term set_iri database t in
           return @@ El.(li [ type_el ]))
    >|= fun els -> El.(ul ~at:[ UIKit.list ] els)

  let submenu _xmpp _iri =
    El.(
      ul
        ~at:At.[ class' @@ Jstr.v "uk-subnav" ]
        [
          li ~at:[ UIKit.Width.expand ] [];
          (* li
           *   [
           *     Evf.on_el Ev.click (fun _ ->
           *         ignore @@ Activity.Publish.like xmpp iri)
           *     @@ a ~at:[ UIKit.Icon.star ] [];
           *   ]; *)
        ])

  let view database xmpp (history : History.t) iri =
    let* description = Database.description database @@ Rdf.Term.of_iri iri in

    let subject_term =
      S.map
        (fun description ->
          Rdf.Description.subject description |> Rdf.Triple.Subject.to_term)
        description
    in

    let* backlinks = S.bind_s subject_term (backlinks database) in
    let* rdfs_types = S.bind_s subject_term (rdfs_types database) in
    S.l4_s
      (fun description backlinks rdfs_types history_stack ->
        let* ddl =
          description_list_of_description history.push database description
        in
        let* backlinks_el = view_backlinks history.push database backlinks in
        let* rdf_types_el = view_rdfs_types history.push database rdfs_types in
        let* title_el =
          title_of_description history.push database description
        in
        let* past_els =
          Lwt_list.mapi_s
            (fun c iri ->
              let* iri_el =
                Ui_rdf.iri (fun _ -> history.take (c + 1)) database iri
              in
              return @@ El.(li [ iri_el ]))
            (List.rev history_stack)
        in
        return
        @@ El.
             [
               div ~at:[ UIKit.Offcanvas.bar ]
                 [
                   ul ~at:[ UIKit.breadcrumb ] past_els;
                   article
                     ~at:[ UIKit.article; UIKit.margin ]
                     [
                       button ~at:[ UIKit.Offcanvas.close; UIKit.close ] [];
                       title_el;
                       submenu xmpp iri;
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
                                     UIKit.Accordion.title;
                                     At.href @@ Jstr.v "#";
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
      description backlinks rdfs_types history.stack
end

(* Component *)

open Archi_lwt

type t = {
  database : Database.t;
  xmpp : Xmpp.t;
  history : History.t;
  el : El.t;
  el_s : unit signal;
}

let start _ database xmpp =
  let history = History.make () in
  let el = offcanvas_bar () in
  let* el_s =
    S.bind_s (History.current history) (function
      | Some iri -> Inspect.view database xmpp history iri
      | None -> return @@ S.const [])
    >|= Brr_react.Elr.def_children el
  in

  return_ok { database; xmpp; history; el; el_s }

let stop _t = return_unit

let component =
  Component.using ~start ~stop
    ~dependencies:[ Database.component; Xmpp.component ]

(* Control visibility *)

let show t iri =
  t.history.reset iri;
  UIKit.Offcanvas.show t.el

(* View *)

let view t = t.el
