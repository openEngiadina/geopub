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

let hash s =
  let _, read_cap =
    Eris.(
      encode_string ~convergence_secret:null_convergence_secret
        ~block_size:`Small s)
  in
  Rdf.Iri.of_string @@ Eris.Read_capability.to_urn read_cap

let actor xmpp_client =
  Xmpp.Client.jid xmpp_client >|= fun jid ->
  ("xmpp:" ^ Xmpp.Jid.(to_string @@ bare jid)) |> Rdf.Iri.of_string

(* Namespaces *)
let activitystreams = Namespace.activitystreams
let geo = Namespace.geo

type state = Compose | Publishing

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
    S.map (fun state ->
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
                           add_if (state = Publishing) disabled
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

let view xmpp_client xmpp_rdf =
  let state, update = S.create Compose in

  let* actor = actor xmpp_client in

  let post object' =
    let* id, graph = Create.make ~object' actor in
    update Publishing;
    Xmpp_rdf.Publish.to_activitystreams_pep xmpp_rdf id graph >|= fun _ ->
    update Compose
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
       (Note.view post state)
