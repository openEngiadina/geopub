(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* View recent activity as a timeline *)

open Brr
open Brr_io
open Lwt
open Lwt.Syntax

(* Setup logging *)

let src = Logs.Src.create "GeoPub"

module Log = (val Logs.src_log src : Logs.LOG)

(* Content-addressable RDF *)

module FragmentGraph = Rdf_fragment_graph.Make (struct
  let hash s =
    let digest = Digestif.BLAKE2B.(digest_string s |> to_raw_string) in
    Rdf.Iri.of_string ("urn:blake2b:" ^ Base32.encode_string ~pad:false digest)
end)

module Geopub_namespace = Namespace
module Xmpp = Geopub_xmpp
module Database = Geopub_database

(* Construct a note as RDF *)

let rdf_to_xml rdf =
  let prefixes =
    [ ("as", Namespace.activitystreams ""); ("geo", Namespace.geo "") ]
  in
  let signals = rdf |> Rdf_xml.to_signals ~prefixes in
  let stream = Lwt_stream.of_seq signals in
  Xmlc.Parser.parse_stream Xmlc.Tree.parser stream

let make_note ?latlng content =
  FragmentGraph.(
    empty
    |> add_statement
         (Predicate.of_iri @@ Rdf.Namespace.rdf "type")
         (Object.of_iri @@ Geopub_namespace.activitystreams "Note")
    |> add_statement
         (Predicate.of_iri @@ Geopub_namespace.activitystreams "content")
         (Object.of_literal @@ Rdf.Literal.make_string content)
    |> add_opt_statement
         (Predicate.of_iri @@ Geopub_namespace.geo "lat")
         (Option.map
            (fun latlng ->
              Object.of_literal @@ Rdf.Literal.make_string @@ Float.to_string
              @@ Leaflet.LatLng.lat latlng)
            latlng)
    |> add_opt_statement
         (Predicate.of_iri @@ Geopub_namespace.geo "long")
         (Option.map
            (fun latlng ->
              Object.of_literal @@ Rdf.Literal.make_string @@ Float.to_string
              @@ Leaflet.LatLng.lng latlng)
            latlng))

let make_create ~object' xmpp =
  let* actor = Xmpp.user_iri xmpp in
  let object_id = FragmentGraph.base_subject object' in
  let create_activity =
    FragmentGraph.(
      empty
      |> add_statement Namespace.a
           (Object.of_iri @@ Geopub_namespace.activitystreams "Create")
      |> add_statement
           (Predicate.of_iri @@ Geopub_namespace.activitystreams "actor")
           (Object.of_iri actor)
      |> add_statement
           (Predicate.of_iri @@ Geopub_namespace.activitystreams "object")
           (Object.of_iri object_id)
      |> add_statement
           (Predicate.of_iri @@ Geopub_namespace.activitystreams "published")
           (Object.of_literal
           @@ Rdf.Literal.make
                (Ptime.to_rfc3339 @@ Ptime_clock.now ())
                (Rdf.Namespace.xsd "dateTime")))
  in
  return
    ( FragmentGraph.base_subject create_activity,
      Rdf.Graph.(
        empty
        |> add_seq (FragmentGraph.to_triples object')
        |> add_seq (FragmentGraph.to_triples create_activity)) )

(* UI *)

let view_compose_note ~update ?latlng (model : Model.t) =
  let compose_form =
    El.(
      form
        ~at:At.[ class' @@ Jstr.v "post-compose" ]
        [
          ul
            [
              li
                [
                  label
                    ~at:At.[ for' @@ Jstr.v "post-content" ]
                    [ txt' "content" ];
                  textarea
                    ~at:
                      At.
                        [
                          id @@ Jstr.v "post-content";
                          type' @@ Jstr.v "text";
                          name @@ Jstr.v "post-content";
                        ]
                    [];
                ];
              (match latlng with
              | Some latlng ->
                  li
                    [
                      label
                        ~at:At.[ for' @@ Jstr.v "post-latlng" ]
                        [ txt' "location" ];
                      input
                        ~at:
                          At.
                            [
                              type' @@ Jstr.v "text";
                              id @@ Jstr.v "post-latlng";
                              name @@ Jstr.v "post-latlng";
                              true' @@ Jstr.v "readonly";
                              value
                              @@ Jstr.v
                                   ((Float.to_string
                                   @@ Leaflet.LatLng.lat latlng)
                                   ^ ", " ^ Float.to_string
                                   @@ Leaflet.LatLng.lng latlng);
                            ]
                        ();
                    ]
              | _ -> txt' "");
              li
                [
                  input
                    ~at:At.[ type' @@ Jstr.v "submit"; value @@ Jstr.v "Post" ]
                    ();
                ];
            ];
        ])
  in
  match model.xmpp with
  | Loadable.Loaded xmpp ->
      return
      @@ Ui.on_el ~default:false Form.Ev.submit
           (fun ev ->
             let form_data =
               Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv
               @@ Ev.target ev
             in

             let post_content_value =
               Form.Data.find form_data (Jstr.v "post-content") |> Option.get
             in

             let content =
               match post_content_value with
               | `String js -> Jstr.to_string js
               | _ -> failwith "We need better error handling"
             in

             update (fun model ->
                 let note = make_note ?latlng content in
                 let* id, activity = make_create ~object':note xmpp in
                 let* xml = rdf_to_xml activity in
                 let* _response = Xmpp.publish_activitystreams xmpp id xml in
                 return model))
           compose_form
  | _ ->
      return
        El.(
          p
            ~at:At.[ class' @@ Jstr.v "meta" ]
            [
              a
                ~at:At.[ href @@ Jstr.v "#settings" ]
                [ txt' "Connect with XMPP" ];
              txt' " to create new posts.";
            ])

let option_bind f opt = match opt with Some v -> f v | None -> None

let functional_property_description database property description =
  match
    Rdf.Description.functional_property property description
    |> option_bind Rdf.Triple.Object.to_iri
  with
  | Some iri -> Database.get_description database iri >|= Option.some
  | None -> return_none

let view_object (model : Model.t) object' =
  let type' =
    Rdf.Description.functional_property
      (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdf "type")
      object'
    |> option_bind Rdf.Triple.Object.to_iri
  in
  match type' with
  | Some type' when type' = Namespace.activitystreams "Note" ->
      let content =
        Rdf.Description.functional_property
          (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "content")
          object'
        |> Option.value
             ~default:
               (Rdf.Triple.Object.of_literal @@ Rdf.Literal.make_string "")
      in
      Inspect.view_object model.database content
  | _ -> Inspect.view_subject model.database (Rdf.Description.subject object')

let view_activity_object (model : Model.t) activity =
  let* object' =
    functional_property_description model.database
      (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "object")
      activity
  in
  match object' with
  | Some object' -> view_object model object'
  | None -> return @@ El.txt' ""

let view_activity (model : Model.t) activity =
  let iri = Rdf.Description.subject activity |> Rdf.Triple.Subject.to_iri in
  match iri with
  | Some iri ->
      let* from =
        Rdf.Description.functional_property
          (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "actor")
          activity
        |> Option.map (Inspect.view_object model.database)
        |> Option.value ~default:(return @@ El.txt' "")
      in
      let* published =
        Rdf.Description.functional_property
          (Rdf.Triple.Predicate.of_iri @@ Namespace.activitystreams "published")
          activity
        |> Option.map (Inspect.view_object model.database)
        |> Option.value ~default:(return @@ El.txt' "")
      in
      let* object_el = view_activity_object model activity in
      return_some
        El.(
          li
            [
              article
                ~at:At.[ class' @@ Jstr.v "activity" ]
                [
                  header
                    [
                      div ~at:At.[ class' @@ Jstr.v "post-from" ] [ from ];
                      div ~at:At.[ class' @@ Jstr.v "post-date" ] [ published ];
                    ];
                  object_el;
                  footer
                    [
                      div
                        ~at:At.[ class' @@ Jstr.v "post-inspect" ]
                        [
                          a
                            ~at:
                              At.[ href @@ Route.to_jstr @@ Route.Inspect iri ]
                            [ txt' "inspect activity" ];
                        ];
                    ];
                ];
            ])
  | None -> return_none

let view ~update model =
  let* compose_note = view_compose_note ~update model in
  let* activities =
    Database.get_activities model.database
    >|= Database.Datalog.Tuple.Set.to_seq
    >|= Seq.filter_map (function
          | [ term ] ->
              Rdf.Term.map term Option.some (fun _ -> None) (fun _ -> None)
          | _ -> None)
    >|= List.of_seq
    >>= Lwt_list.map_s (fun iri -> Database.get_description model.database iri)
  in
  let* activities_lis =
    Lwt_list.filter_map_s (view_activity model) activities
  in
  return
    El.(
      div
        ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
        [
          h1 [ txt' "Activity" ];
          compose_note;
          ul ~at:At.[ class' @@ Jstr.v "activity" ] activities_lis;
        ])
