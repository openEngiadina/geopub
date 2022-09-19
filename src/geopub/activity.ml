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

  let create_with_actor xmpp objects_f =
    let* actor = actor xmpp in
    let object' = objects_f actor in
    let* id, graph = Create.make [ object' ] actor in
    to_activitystreams_pep xmpp id graph

  let like xmpp iri =
    let* actor = actor xmpp in
    let* id, graph = Like.make ~actor iri in
    to_activitystreams_pep xmpp id graph
end

(* Helpers to get data from forms *)
module Fd = struct
  let get_string form_data field_name =
    Option.bind
      (Form.Data.find form_data (Jstr.v field_name))
      (function `String js -> Option.some @@ Jstr.to_string js | _ -> None)

  let get_string_literal form_data field_name =
    Option.map Rdf.Literal.make_string (get_string form_data field_name)
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

  let view xmpp latlng =
    El.(
      Evf.on_el ~default:false Form.Ev.submit (fun ev ->
          let form = Ev.(target_to_jv @@ target ev) in
          let form_data =
            Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
          in

          let content = Fd.get_string form_data "content" in

          match content with
          | Some content ->
              let note = make ?latlng content in

              ignore @@ Jv.call form "reset" [||];
              ignore @@ Publish.create xmpp [ note ]
          | None -> ())
      @@ form
           ~at:[ UIKit.Form.stacked; UIKit.margin ]
           [
             (* Content *)
             textarea
               ~at:
                 At.
                   [
                     UIKit.Form.input;
                     UIKit.Form.textarea;
                     UIKit.Height.small;
                     id @@ Jstr.v "content";
                     name @@ Jstr.v "content";
                   ]
               [];
             div ~at:[ UIKit.margin ]
               (match latlng with
               | Some latlng ->
                   [
                     label
                       ~at:At.[ UIKit.Form.label; for' @@ Jstr.v "post-latlng" ]
                       [ txt' "Location" ];
                     input
                       ~at:
                         At.
                           [
                             UIKit.Form.input;
                             UIKit.Form.controls;
                             type' @@ Jstr.v "text";
                             id @@ Jstr.v "post-latlng";
                             name @@ Jstr.v "post-latlng";
                             true' @@ Jstr.v "readonly";
                             value
                             @@ Jstr.v
                                  ((Float.to_string @@ Leaflet.Latlng.lat latlng)
                                  ^ ", " ^ Float.to_string
                                  @@ Leaflet.Latlng.lng latlng);
                           ]
                       ();
                   ]
               | _ -> [ txt' "" ]);
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

module ValueFlows = struct
  let vf = Rdf.Namespace.make_namespace "https://w3id.org/valueflows#"
  let dc = Namespace.dc

  module Offer = struct
    let make ?latlng proposal_title proposal_description intent_title
        intent_description actor =
      Rdf_cbor.Content_addressable.(
        empty
        |> add_statement Namespace.a (Object.of_iri @@ vf "Proposal")
        |> add_opt_statement
             (Predicate.of_iri @@ dc "title")
             (Option.map Object.of_literal proposal_title)
        |> add_opt_statement
             (Predicate.of_iri @@ dc "description")
             (Option.map Object.of_literal proposal_description)
        |> add_statement
             (Predicate.of_iri @@ vf "intent")
             (Object.make_fragment_reference "intent")
        (* Intent *)
        |> add_fragment_statement "intent" Namespace.a
             (Object.of_iri @@ vf "Intent")
        |> add_opt_fragment_statement "intent"
             (Predicate.of_iri @@ dc "title")
             (Option.map Object.of_literal intent_title)
        |> add_opt_fragment_statement "intent"
             (Predicate.of_iri @@ dc "description")
             (Option.map Object.of_literal intent_description)
        |> add_fragment_statement "intent"
             (Predicate.of_iri @@ vf "provider")
             (Object.of_iri actor)
           (* Lat/Long *)
        |> add_opt_statement
             (Predicate.of_iri @@ geo "lat")
             (Option.map
                (fun latlng ->
                  Object.of_literal @@ Rdf.Literal.make_string
                  @@ Float.to_string @@ Leaflet.Latlng.lat latlng)
                latlng)
        |> add_opt_statement
             (Predicate.of_iri @@ geo "long")
             (Option.map
                (fun latlng ->
                  Object.of_literal @@ Rdf.Literal.make_string
                  @@ Float.to_string @@ Leaflet.Latlng.lng latlng)
                latlng))

    let view xmpp latlng =
      El.(
        Evf.on_el ~default:false Form.Ev.submit (fun ev ->
            let form = Ev.(target_to_jv @@ target ev) in
            let form_data =
              Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
            in

            let proposal_title =
              Fd.get_string_literal form_data "proposal-title"
            in
            let proposal_description =
              Fd.get_string_literal form_data "proposal-description"
            in

            let intent_title = Fd.get_string_literal form_data "intent-title" in
            let intent_description =
              Fd.get_string_literal form_data "intent-description"
            in

            let proposal =
              make ?latlng proposal_title proposal_description intent_title
                intent_description
            in

            ignore @@ Jv.call form "reset" [||];
            ignore @@ Publish.create_with_actor xmpp proposal)
        @@ form
             ~at:[ UIKit.Form.stacked; UIKit.margin ]
             [
               (* title *)
               div ~at:[ UIKit.margin ]
                 [
                   label
                     ~at:
                       At.[ UIKit.Form.label; for' @@ Jstr.v "proposal-title" ]
                     [ txt' "Title" ];
                   input
                     ~at:
                       At.
                         [
                           UIKit.Form.input;
                           UIKit.Form.controls;
                           id @@ Jstr.v "proposal-title";
                           name @@ Jstr.v "proposal-title";
                           type' @@ Jstr.v "text";
                         ]
                     ();
                 ];
               (* description *)
               div ~at:[ UIKit.margin ]
                 [
                   label
                     ~at:
                       At.
                         [
                           UIKit.Form.label;
                           for' @@ Jstr.v "proposal-description";
                         ]
                     [
                       txt' "Short description of what you are offering and why";
                     ];
                   textarea
                     ~at:
                       At.
                         [
                           UIKit.Form.input;
                           UIKit.Height.small;
                           UIKit.Form.controls;
                           id @@ Jstr.v "proposal-descriptoin";
                           name @@ Jstr.v "proposal-description";
                         ]
                     [];
                 ];
               (* Location *)
               div ~at:[ UIKit.margin ]
                 (match latlng with
                 | Some latlng ->
                     [
                       label
                         ~at:
                           At.[ UIKit.Form.label; for' @@ Jstr.v "post-latlng" ]
                         [ txt' "Location" ];
                       input
                         ~at:
                           At.
                             [
                               UIKit.Form.input;
                               UIKit.Form.controls;
                               type' @@ Jstr.v "text";
                               id @@ Jstr.v "post-latlng";
                               name @@ Jstr.v "post-latlng";
                               true' @@ Jstr.v "readonly";
                               value
                               @@ Jstr.v
                                    ((Float.to_string
                                    @@ Leaflet.Latlng.lat latlng)
                                    ^ ", " ^ Float.to_string
                                    @@ Leaflet.Latlng.lng latlng);
                             ]
                         ();
                     ]
                 | _ -> [ txt' "" ]);
               (* Intent *)
               (* Intent title *)
               div ~at:[ UIKit.margin ]
                 [
                   label
                     ~at:At.[ UIKit.Form.label; for' @@ Jstr.v "intent-title" ]
                     [ txt' "Short title of the ressource you are offering" ];
                   input
                     ~at:
                       At.
                         [
                           UIKit.Form.input;
                           UIKit.Form.controls;
                           id @@ Jstr.v "intent-title";
                           name @@ Jstr.v "intent-title";
                           type' @@ Jstr.v "text";
                         ]
                     ();
                 ];
               (* intent-description *)
               div ~at:[ UIKit.margin ]
                 [
                   label
                     ~at:
                       At.
                         [
                           UIKit.Form.label; for' @@ Jstr.v "intent-description";
                         ]
                     [ txt' "Description of the resource you are offerng" ];
                   textarea
                     ~at:
                       At.
                         [
                           UIKit.Form.controls;
                           UIKit.Height.medium;
                           UIKit.Form.input;
                           id @@ Jstr.v "intent-descripiton";
                           name @@ Jstr.v "intent-description";
                         ]
                     [];
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
end

module Turtle = struct
  let default =
    {rdf|@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix vf: <https://w3id.org/valueflows#> .
@prefix om2:   <http://www.ontology-of-units-of-measure.org/resource/om-2/> .

<>
   a vf:ProposedIntent ;
   vf:publishedIn <#proposal> ;
   vf:publishes <#offer> ;
   vf:publishes <#payment> .

<#proposal>
   a vf:Proposal ;
   vf:name "Offer something for 10 euro" .

<#offer>
   a vf:Intent ;
   vf:name "Something" ;
   vf:action vf:Transfer ;
   vf:provider <xmpp:pukkamustard@posteo.net> .

<#payment>
  a vf:Intent ;
  vf:receiver <xmpp:pukkamustard@posteo.net> ;
  vf:resourceQuantity <#paymentMeasure> .

<#paymentMeasure>
  a om2:Measure ;
  om2:hasUnit om2:euro ;
  om2:hasNumericalValue 10 .|rdf}

  let view xmpp =
    El.(
      Evf.on_el ~default:false Form.Ev.submit (fun ev ->
          let form = Ev.(target_to_jv @@ target ev) in
          let form_data =
            Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
          in

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
  let view xmpp latlng =
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
                         ~at:At.(add_if (input_type = `Offer) UIKit.active [])
                         [
                           Evf.on_el Ev.click (fun _ -> set_input_type `Offer)
                           @@ a [ txt' "ValueFlows Offer" ];
                         ];
                       li
                         ~at:At.(add_if (input_type = `Turtle) UIKit.active [])
                         [
                           Evf.on_el Ev.click (fun _ -> set_input_type `Turtle)
                           @@ a [ txt' "RDF/Turtle" ];
                         ];
                     ];
                   (match input_type with
                   | `Note -> Note.view xmpp latlng
                   | `Offer -> ValueFlows.Offer.view xmpp latlng
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

let view_activity inspect xmpp db description =
  let subject_iri =
    Rdf.Description.subject description
    |> Rdf.Triple.Subject.map (fun iri -> Some iri) (fun _ -> None)
  in

  let* from =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "actor")
      description
    |> Option.map (Ui_rdf.object' inspect db)
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
    |> Option.map (Ui_rdf.object' ?href:subject_iri inspect db)
    |> Option.value ~default:(return @@ El.txt' "")
  in
  let* published =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "published")
      description
    |> Option.map (Ui_rdf.object' inspect db)
    |> Option.value ~default:(return @@ El.txt' "")
  in
  let object_term =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "object")
      description
  in
  let* object_el =
    object_term
    |> Option.map (Ui_rdf.object' inspect db)
    |> Option.value ~default:(return @@ El.txt' "")
  in

  let object_iri = Option.bind object_term Rdf.Triple.Object.to_iri in

  let* content_el =
    let as_content iri =
      Database.get_functional_property db
        (Rdf.Triple.Subject.of_iri iri)
        (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "content")
    in

    let dc_description iri =
      Database.get_functional_property db
        (Rdf.Triple.Subject.of_iri iri)
        (Rdf.Triple.Predicate.of_iri @@ Namespace.dc "description")
    in

    match object_iri with
    | Some iri -> (
        as_content iri >>= function
        | Some o -> Ui_rdf.object' inspect db o
        | None -> dc_description iri >>= Ui_rdf.object_option inspect db)
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

let view inspector xmpp db latlng =
  let xmpp_client =
    Xmpp.(Connection.client_signal @@ connection xmpp)
    |> S.map Loadable.to_option
  in

  let inspect iri = Inspector.show inspector iri in

  let* new_post_view =
    S.bind_s xmpp_client (fun xmpp_client ->
        match xmpp_client with
        | Some _xmpp_client ->
            Compose.view xmpp latlng |> return
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
  >>= S.map_s (Lwt_list.map_s @@ view_activity inspect xmpp db)
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
