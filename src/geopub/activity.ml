(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Lwt_react
open Brr
open Brr_io
open Brr_react

(* Setup logging *)

let src = Logs.Src.create "GeoPub.Activity"

module Log = (val Logs.src_log src : Logs.LOG)

(* Namespaces *)
let activitystreams = Namespace.activitystreams
let geo = Namespace.geo

(* Content-addressing with ERIS *)

let hash s =
  let _, read_cap =
    Eris.(
      encode_string ~convergence_secret:null_convergence_secret
        ~block_size:`Small s)
  in
  Rdf.Iri.of_string @@ Eris.Read_capability.to_urn read_cap

(* Activity types *)

module Create = struct
  let make ~object' actor =
    let object_id = Rdf_cbor.Content_addressable.base_subject ~hash object' in
    let create_activity =
      Rdf_cbor.Content_addressable.(
        empty
        |> add_statement Namespace.a (Object.of_iri @@ activitystreams "Create")
        |> add_statement
             (Predicate.of_iri @@ activitystreams "actor")
             (Object.of_iri actor)
        |> add_statement
             (Predicate.of_iri @@ activitystreams "object")
             (Object.of_iri object_id)
        |> add_statement
             (Predicate.of_iri @@ activitystreams "published")
             (Object.of_literal
             @@ Rdf.Literal.make
                  (Ptime.to_rfc3339 @@ Ptime_clock.now ())
                  (Rdf.Namespace.xsd "dateTime")))
    in
    return
      ( Rdf_cbor.Content_addressable.base_subject ~hash create_activity,
        Rdf.Graph.(
          empty
          |> add_seq (Rdf_cbor.Content_addressable.to_triples ~hash object')
          |> add_seq
               (Rdf_cbor.Content_addressable.to_triples ~hash create_activity))
      )
end

module Like = struct
  let make ~actor iri =
    let like_activity =
      Rdf_cbor.Content_addressable.(
        empty
        |> add_statement Namespace.a (Object.of_iri @@ activitystreams "Like")
        |> add_statement
             (Predicate.of_iri @@ activitystreams "actor")
             (Object.of_iri actor)
        |> add_statement
             (Predicate.of_iri @@ activitystreams "object")
             (Object.of_iri iri)
        |> add_statement
             (Predicate.of_iri @@ activitystreams "published")
             (Object.of_literal
             @@ Rdf.Literal.make
                  (Ptime.to_rfc3339 @@ Ptime_clock.now ())
                  (Rdf.Namespace.xsd "dateTime")))
    in
    return
      ( Rdf_cbor.Content_addressable.base_subject ~hash like_activity,
        Rdf.Graph.(
          empty
          |> add_seq
               (Rdf_cbor.Content_addressable.to_triples ~hash like_activity)) )
end

(* Publish Activities *)

module Publish = struct
  module Client = Xmppl_websocket.Client
  module Pubsub = Xmppl_pubsub.Make (Client)

  let actor xmpp =
    let xmpp_client = Xmpp.(Connection.client @@ connection xmpp) in
    match xmpp_client with
    | Ok xmpp_client ->
        Xmpp.Client.jid xmpp_client >|= fun jid ->
        ("xmpp:" ^ Xmpp.Jid.(to_string @@ bare jid)) |> Rdf.Iri.of_string
    | Error _ -> return @@ Rdf.Iri.of_string "urn:not-connected"

  let rdf_to_xml rdf =
    let prefixes =
      [ ("as", Namespace.activitystreams ""); ("geo", Namespace.geo "") ]
    in
    let signals = rdf |> Rdf_xml.to_signals ~prefixes in
    let stream = Lwt_stream.of_seq signals in
    Xmlc.Parser.parse_stream Xmlc.Tree.parser stream

  let to_activitystreams_pep t id graph =
    let* client =
      Xmpp.connection t |> Xmpp.Connection.client |> function
      | Ok client -> return client
      | _ -> fail_with "no XMPP client"
    in
    let* jid = Xmpp.Client.jid client in
    let* xml = rdf_to_xml graph in
    let item =
      Xmlc.Tree.make_element
        ~attributes:[ (("", "id"), Rdf.Iri.to_string id) ]
        ~children:[ xml ]
        (Pubsub.Namespace.pubsub "item")
    in
    Pubsub.publish ~to':(Xmpp.Jid.bare jid)
      ~node:"net.openengiadina.xmpp.activitystreams" client (Some item)

  let like xmpp iri =
    let* actor = actor xmpp in
    let* id, graph = Like.make ~actor iri in
    to_activitystreams_pep xmpp id graph
end

(* Object types *)

module Note = struct
  let make ?latlng content =
    Rdf_cbor.Content_addressable.(
      empty
      |> add_statement
           (Predicate.of_iri @@ Rdf.Namespace.rdf "type")
           (Object.of_iri @@ activitystreams "Note")
      |> add_statement
           (Predicate.of_iri @@ activitystreams "content")
           (Object.of_literal @@ Rdf.Literal.make_string content)
      |> add_opt_statement
           (Predicate.of_iri @@ geo "lat")
           (Option.map
              (fun latlng ->
                Object.of_literal @@ Rdf.Literal.make_string @@ Float.to_string
                @@ Leaflet.Latlng.lat latlng)
              latlng)
      |> add_opt_statement
           (Predicate.of_iri @@ geo "long")
           (Option.map
              (fun latlng ->
                Object.of_literal @@ Rdf.Literal.make_string @@ Float.to_string
                @@ Leaflet.Latlng.lng latlng)
              latlng))

  let view post =
    S.map (fun busy ->
        El.(
          Evf.on_el ~default:false Form.Ev.submit (fun ev ->
              (* let form = Ev.(target_to_jv @@ target ev) in *)
              let form_data =
                Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv
                @@ Ev.target ev
              in

              Console.log [ form_data ];

              let content_value =
                Form.Data.find form_data (Jstr.v "content") |> Option.get
              in

              let content =
                match content_value with
                | `String js -> Jstr.to_string js
                | _ -> failwith "We need better error handling"
              in

              let note = make content in
              ignore @@ post note)
          @@ form
               ~at:[ UIKit.Form.stacked; UIKit.margin ]
               [
                 (* Content *)
                 textarea
                   ~at:
                     At.
                       [
                         UIKit.Form.textarea;
                         UIKit.Form.controls;
                         id @@ Jstr.v "content";
                         name @@ Jstr.v "content";
                       ]
                   [];
                 (* Post *)
                 div ~at:[ UIKit.margin ]
                   [
                     input
                       ~at:
                         At.(
                           add_if busy disabled
                             [
                               UIKit.Form.input;
                               UIKit.Button.primary;
                               id @@ Jstr.v "submit";
                               type' @@ Jstr.v "submit";
                               value @@ Jstr.v "Post";
                             ])
                       ();
                   ];
               ]))
end

module Compose = struct
  let view xmpp =
    let busy_s, set_busy = S.create false in

    let* actor = Publish.actor xmpp in

    let post object' =
      let* id, graph = Create.make ~object' actor in
      set_busy true;
      Publish.to_activitystreams_pep xmpp id graph >|= fun _ -> set_busy false
    in

    return
    @@ S.map
         (fun compose_el ->
           El.
             [
               div ~at:[ UIKit.container ]
                 [
                   h3 [ txt' "New Post" ];
                   ul ~at:[ UIKit.subnav ]
                     [
                       li ~at:[ UIKit.active ]
                         [ a [ txt' "ActivityStreams Note" ] ];
                       li [ a [ txt' "ValueFlows Proposal" ] ];
                       li [ a [ txt' "RDF" ] ];
                     ];
                   compose_el;
                 ];
             ])
         (Note.view post busy_s)
end

(* Query for Activities *)

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

let view_activity xmpp db description =
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
                       li
                         [
                           Evf.on_el Ev.click (fun _ ->
                               match subject_iri with
                               | Some iri -> ignore @@ Publish.like xmpp iri
                               | None -> ())
                           @@ a ~at:[ UIKit.Icon.star ] [];
                         ];
                     ];
                 ];
               div ~at:[ UIKit.Comment.body ] [ content_el ];
             ];
         ])

let view xmpp db =
  let xmpp_client =
    Xmpp.(Connection.client_signal @@ connection xmpp)
    |> S.map Loadable.to_option
  in

  let* new_post_view =
    S.bind_s xmpp_client (fun xmpp_client ->
        match xmpp_client with
        | Some _xmpp_client ->
            Compose.view xmpp
            >|= S.map (fun els ->
                    El.
                      [
                        div
                          ~at:
                            [
                              UIKit.section; UIKit.Section.muted; UIKit.padding;
                            ]
                          els;
                      ])
        | None ->
            return @@ S.const
            @@ El.
                 [
                   div ~at:[ UIKit.alert ]
                     [ p [ txt' "Login to post content" ] ];
                 ])
  in

  activities db
  >>= S.map_s (Lwt_list.map_s @@ view_activity xmpp db)
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
