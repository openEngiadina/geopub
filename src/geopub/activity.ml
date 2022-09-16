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
  let make os actor =
    let object_ids =
      List.map (Rdf_cbor.Content_addressable.base_subject ~hash) os
    in
    let create_activity =
      Rdf_cbor.Content_addressable.(
        empty
        |> add_statement Namespace.a (Object.of_iri @@ activitystreams "Create")
        |> add_statement
             (Predicate.of_iri @@ activitystreams "actor")
             (Object.of_iri actor)
        |> add_statement
             (Predicate.of_iri @@ activitystreams "published")
             (Object.of_literal
             @@ Rdf.Literal.make
                  (Ptime.to_rfc3339 @@ Ptime_clock.now ())
                  (Rdf.Namespace.xsd "dateTime"))
        |> fun fragment ->
        List.fold_left
          (fun fragment object_id ->
            add_statement
              (Predicate.of_iri @@ activitystreams "object")
              (Object.of_iri object_id) fragment)
          fragment object_ids)
    in
    return
      ( Rdf_cbor.Content_addressable.base_subject ~hash create_activity,
        Rdf.Graph.(
          empty
          |> add_seq
               (os |> List.to_seq
               |> Seq.concat_map (Rdf_cbor.Content_addressable.to_triples ~hash)
               )
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

  let create xmpp objects =
    let* actor = actor xmpp in
    let* id, graph = Create.make objects actor in
    to_activitystreams_pep xmpp id graph

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

  let view xmpp =
    El.(
      Evf.on_el ~default:false Form.Ev.submit (fun ev ->
          let form = Ev.(target_to_jv @@ target ev) in
          let form_data =
            Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
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

          ignore @@ Jv.call form "reset" [||];
          ignore @@ Publish.create xmpp [ note ])
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
                     At.
                       [
                         UIKit.Form.input;
                         UIKit.Button.primary;
                         id @@ Jstr.v "submit";
                         type' @@ Jstr.v "submit";
                         value @@ Jstr.v "Post";
                       ]
                   ();
               ];
           ])
end

module Turtle = struct
  let default =
    {rdf|@prefix as: <https://www.w3.org/ns/activitystreams#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix geo: <http://www.w3.org/2003/01/geo/wgs84_pos#> .

<>
  a as:Note ;
  geo:lat "46.7970040956";
  geo:long "10.2982868244";
  as:content "Hi!"@en;
  as:content "Hello!"@de;
  as:content "Salut!"@fr;
  as:content "Grüezi!"@gsw;
  as:content "Allegra!"@rm .
|rdf}

  let view xmpp =
    El.(
      Evf.on_el ~default:false Form.Ev.submit (fun ev ->
          let form = Ev.(target_to_jv @@ target ev) in
          let form_data =
            Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
          in

          Console.log [ form_data ];

          let content_value =
            Form.Data.find form_data (Jstr.v "turtle-content") |> Option.get
          in

          let content =
            match content_value with
            | `String js -> Jstr.to_string js
            | _ -> failwith "We need better error handling"
          in

          let objects =
            content |> String.to_seq |> Rdf_turtle.parse_to_graph
            |> Rdf.Graph.to_triples |> Rdf_cbor.Content_addressable.of_triples
            |> Seq.map snd |> List.of_seq
          in

          ignore @@ Jv.call form "reset" [||];
          ignore @@ Publish.create xmpp objects)
      @@ form
           ~at:[ UIKit.Form.stacked; UIKit.margin ]
           [
             (* Content *)
             div
               [
                 label
                   ~at:At.[ UIKit.Form.label; for' @@ Jstr.v "turtle-content" ]
                   [ txt' "RDF/Turtle" ];
                 textarea
                   ~at:
                     At.
                       [
                         UIKit.Form.textarea;
                         UIKit.Form.controls;
                         UIKit.Height.medium;
                         id @@ Jstr.v "turtle-content";
                         name @@ Jstr.v "turtle-content";
                       ]
                   [ txt' default ];
               ];
             (* Post *)
             div ~at:[ UIKit.margin ]
               [
                 input
                   ~at:
                     At.
                       [
                         UIKit.Form.input;
                         UIKit.Button.primary;
                         id @@ Jstr.v "submit";
                         type' @@ Jstr.v "submit";
                         value @@ Jstr.v "Post";
                       ]
                   ();
               ];
           ])
end

module Compose = struct
  let view xmpp =
    let input_type_s, set_input_type = S.create `Note in

    input_type_s
    |> S.map (fun input_type ->
           El.
             [
               div ~at:[ UIKit.container ]
                 [
                   h3 [ txt' "New Post" ];
                   ul ~at:[ UIKit.subnav ]
                     [
                       li
                         ~at:At.(add_if (input_type = `Note) UIKit.active [])
                         [
                           Evf.on_el Ev.click (fun _ -> set_input_type `Note)
                           @@ a [ txt' "ActivityStreams Note" ];
                         ];
                       li
                         ~at:
                           At.(add_if (input_type = `Proposal) UIKit.active [])
                         [
                           Evf.on_el Ev.click (fun _ ->
                               set_input_type `Proposal)
                           @@ a [ txt' "ValueFlows Proposal" ];
                         ];
                       li
                         ~at:At.(add_if (input_type = `Turtle) UIKit.active [])
                         [
                           Evf.on_el Ev.click (fun _ -> set_input_type `Turtle)
                           @@ a [ txt' "RDF/Turtle" ];
                         ];
                     ];
                   (match input_type with
                   | `Note -> Note.view xmpp
                   | `Proposal -> txt' "TODO"
                   | `Turtle -> Turtle.view xmpp);
                 ];
             ])
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

  let from_iri =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "actor")
      description
    |> fun o ->
    Option.bind o
      (Rdf.Triple.Object.map
         (fun iri -> Some iri)
         (fun _ -> None)
         (fun _ -> None))
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
                       (match from_iri with
                       | Some iri ->
                           li
                             [
                               a
                                 ~at:
                                   At.
                                     [
                                       title @@ Jstr.v "Send direct message";
                                       href @@ Jstr.v @@ Rdf.Iri.to_string iri;
                                       UIKit.Icon.mail;
                                     ]
                                 [];
                             ]
                       | None -> txt' "");
                       li
                         [
                           Evf.on_el Ev.click (fun _ ->
                               match subject_iri with
                               | Some iri -> ignore @@ Publish.like xmpp iri
                               | None -> ())
                           @@ a
                                ~at:
                                  [ At.title @@ Jstr.v "Like"; UIKit.Icon.star ]
                                [];
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
            Compose.view xmpp |> return
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
