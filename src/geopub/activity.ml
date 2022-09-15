(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Lwt_react
open Brr

(* Setup logging *)

let src = Logs.Src.create "GeoPub.Activity"

module Log = (val Logs.src_log src : Logs.LOG)

(* Component *)

module TermSet = Set.Make (Rdf.Term)

let sort_activities =
  let published activity =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "published")
      activity
  in
  List.sort (fun a b ->
      Option.compare Rdf.Triple.Object.compare (published b) (published a))

let activities db =
  let query =
    Database.Datalog.(
      Atom.make "triple-rhodf"
        Term.
          [
            make_variable "s";
            make_constant
            @@ Constant.Rdf (Rdf.Term.of_iri @@ Rdf.Namespace.rdf "type");
            make_constant
            @@ Constant.Rdf
                 (Rdf.Term.of_iri @@ Namespace.activitystreams "Activity");
          ])
  in

  Database.query db query
  >>= S.map_s ~eq:(List.equal Rdf.Description.equal) (fun (_tx, set) ->
          Database.Datalog.Tuple.Set.to_seq set
          |> Seq.filter_map (function
               | [ Database.Datalog.Constant.Rdf s; _; _ ] -> Some s
               | _ -> None)
          |> Lwt_seq.of_seq
          |> Lwt_seq.map_s (Database.get_description db)
          |> Lwt_seq.to_list >|= sort_activities)

let view_activity db description =
  let subject_iri =
    Rdf.Description.subject description
    |> Rdf.Triple.Subject.map (fun iri -> Some iri) (fun _ -> None)
  in

  let* from =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "actor")
      description
    |> Option.map (Ui_rdf.object' db)
    |> Option.value ~default:(return @@ El.txt' "")
  in
  let* type_el =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdf "type")
      description
    |> Option.map (Ui_rdf.object' ?href:subject_iri db)
    |> Option.value ~default:(return @@ El.txt' "")
  in
  let* published =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "published")
      description
    |> Option.map (Ui_rdf.object' db)
    |> Option.value ~default:(return @@ El.txt' "")
  in
  let object_term =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "object")
      description
  in
  let* object_el =
    object_term
    |> Option.map (Ui_rdf.object' db)
    |> Option.value ~default:(return @@ El.txt' "")
  in

  let object_iri = Option.bind object_term Rdf.Triple.Object.to_iri in

  let* content_el =
    match object_iri with
    | Some iri -> (
        Database.get_functional_property db
          (Rdf.Triple.Subject.of_iri iri)
          (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "content")
        >>= function
        | Some o -> Ui_rdf.object' db o
        | None -> return @@ El.txt' "")
    | None -> return @@ El.txt' ""
  in

  return
  @@ El.(
       li
         [
           article
             ~at:[ UIKit.comment; UIKit.Comment.primary; UIKit.margin ]
             [
               header ~at:[ UIKit.Comment.header ]
                 [
                   h4 ~at:[ UIKit.Comment.title ] [ from ];
                   ul
                     ~at:
                       [
                         UIKit.Comment.meta; UIKit.subnav; UIKit.Subnav.divider;
                       ]
                     [
                       li [ type_el ];
                       li [ object_el ];
                       li [ published ];
                       li ~at:[ UIKit.Width.expand ] [];
                       li [ a ~at:[ UIKit.Icon.comment ] [] ];
                       (* li [ a ~at:[] [ txt' "Like" ] ]; *)
                     ];
                 ];
               div ~at:[ UIKit.Comment.body ] [ content_el ];
             ];
         ])

let view xmpp xmpp_rdf db =
  let xmpp_client =
    Xmpp.(Connection.client_signal @@ connection xmpp)
    |> S.map Loadable.to_option
  in

  let* new_post_view =
    S.bind_s xmpp_client (fun xmpp_client ->
        match xmpp_client with
        | Some xmpp_client ->
            (* div ~at:[ UIKit.section; UIKit.Section.muted; UIKit.padding ] *)
            New_post.view xmpp_client xmpp_rdf
        | None ->
            return @@ S.const
            @@ El.
                 [
                   div ~at:[ UIKit.alert ]
                     [ p [ txt' "Login to post content" ] ];
                 ])
  in

  activities db
  >>= S.map_s (Lwt_list.map_s @@ view_activity db)
  >|= S.l2
        (fun new_post_el cs ->
          El.
            [
              div
                ~at:[ UIKit.container; UIKit.margin ]
                [
                  h1 [ txt' "Activity" ];
                  div @@ new_post_el;
                  hr ();
                  ul ~at:[ UIKit.Comment.list ] cs;
                ];
            ])
        new_post_view
