(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* ActivityStreams 2.0 support *)

open Brr
open Brr_io
open Reactor_brr

(* Namespace *)

let as' = Rdf.Namespace.make_namespace "http://www.w3.org/ns/activitystreams"

let geo =
  Rdf.Namespace.make_namespace "http://www.w3.org/2003/01/geo/wgs84_pos#"

module Note = struct
  type t = { id : Rdf.Iri.t; content : string; geoloc : Geoloc.t option }

  let to_rdf note =
    Rdf.(
      Graph.empty
      |> Graph.add
           Triple.(
             make (Subject.of_iri note.id)
               (Predicate.of_iri @@ Rdf.Namespace.rdf "type")
               (Object.of_iri @@ as' "Note"))
      |> Graph.add
           Triple.(
             make (Subject.of_iri note.id)
               (Predicate.of_iri @@ as' "content")
               (Object.of_literal
               @@ Literal.make note.content (Namespace.xsd "string")))
      |> fun graph ->
      match note.geoloc with
      | Some geoloc ->
          graph
          |> Graph.add
               Triple.(
                 make (Subject.of_iri note.id)
                   (Predicate.of_iri @@ geo "lat")
                   (Object.of_literal
                   @@ Literal.make
                        (Float.to_string geoloc.latitude)
                        (Namespace.xsd "string")))
          |> Graph.add
               Triple.(
                 make (Subject.of_iri note.id)
                   (Predicate.of_iri @@ geo "long")
                   (Object.of_literal
                   @@ Literal.make
                        (Float.to_string geoloc.longitude)
                        (Namespace.xsd "string")))
      | None -> graph)

  let as_create jid note =
    (* TODO figure out how to use random generator without initializing always *)
    Random.self_init ();
    let seed = Random.get_state () in
    let uuid = Uuidm.v4_gen seed () |> Uuidm.to_string in
    let id = Rdf.Iri.of_string ("urn:uuid:" ^ uuid) in
    let actor_iri =
      ("xmpp:" ^ Xmpp.Jid.(to_string @@ bare jid)) |> Rdf.Iri.of_string
    in
    ( id,
      Rdf.(
        to_rdf note
        |> Graph.add
             Triple.(
               make (Subject.of_iri id)
                 (Predicate.of_iri @@ Rdf.Namespace.rdf "type")
                 (Object.of_iri @@ as' "Create"))
        |> Graph.add
             Triple.(
               make (Subject.of_iri id)
                 (Predicate.of_iri @@ as' "actor")
                 (Object.of_iri @@ actor_iri))
        |> Graph.add
             Triple.(
               make (Subject.of_iri id)
                 (Predicate.of_iri @@ as' "object")
                 (Object.of_iri @@ note.id))
        |> Graph.add
             Triple.(
               make (Subject.of_iri id)
                 (Predicate.of_iri @@ as' "published")
                 (Object.of_literal
                 @@ Literal.make
                      (Ptime.to_rfc3339 @@ Ptime_clock.now ())
                      (Rdf.Namespace.xsd "dateTime")))) )
end

let rdf_to_xml rdf =
  let prefixes = [ ("as", as' ""); ("geo", geo "") ] in
  let signals = rdf |> Rdf_xml.to_signals ~prefixes in
  let stream = Lwt_stream.of_seq signals in
  Xmlc.Parser.parse_stream Xmlc.Tree.parser stream

let ns_pubsub_event local = ("http://jabber.org/protocol/pubsub#event", local)

let parse (message : Xmpp.Stanza.Message.t) =
  let rdf_parser =
    Xmlc.Parser.(
      element (ns_pubsub_event "event") (fun _ ->
          element (ns_pubsub_event "items") (fun _ ->
              element (ns_pubsub_event "item") (fun _ ->
                  Xmlc.Tree.parser >>| Xmlc.Tree.to_seq
                  >>| Rdf_xml.parse_to_graph))))
  in
  Lwt_result.catch
    (Xmlc.Tree.parse_trees rdf_parser (List.to_seq message.payload))

(* Compose view *)

let view_compose ?latlng ~send_msg =
  let note_form =
    Evr.on_el ~default:false Form.Ev.submit
      (fun ev ->
        let form_data =
          Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
        in

        let post_content_value =
          Form.Data.find form_data (Jstr.v "post-content") |> Option.get
        in

        let content =
          match post_content_value with
          | `String js -> Jstr.to_string js
          | _ -> failwith "We need better error handling"
        in

        let geoloc =
          Option.map
            (fun latlng ->
              Leaflet.LatLng.(Geoloc.make (lat latlng) (lng latlng)))
            latlng
        in

        Random.self_init ();
        let seed = Random.get_state () in
        let uuid = Uuidm.v4_gen seed () |> Uuidm.to_string in
        let id = Rdf.Iri.of_string ("urn:uuid:" ^ uuid) in

        let post : Note.t = { id; content; geoloc } in

        send_msg @@ `PostActivityStreamsNote post)
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
                      ~at:
                        At.[ type' @@ Jstr.v "submit"; value @@ Jstr.v "Post" ]
                      ();
                  ];
              ];
          ])
  in

  El.(
    div
      ~at:At.[ id @@ Jstr.v "action-bar"; class' @@ Jstr.v "content" ]
      [
        h2 [ txt' "New Post" ];
        note_form;
        Evr.on_el Ev.click (fun _ -> send_msg @@ `SetActionBar None)
        @@ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' "Cancel" ];
      ])

module ObjectSet = Set.Make (Rdf.Triple.Object)

(* Warning: Options galore...this needs improvment *)

let activities graph =
  Rdf.Graph.subjects graph
  |> Seq.filter (fun subject ->
         Rdf.Graph.objects subject
           (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdf "type")
           graph
         |> ObjectSet.of_seq
         |> ObjectSet.mem (Rdf.Triple.Object.of_iri @@ as' "Create"))

let seq_next seq =
  match seq () with Seq.Cons (value, _) -> Some value | Seq.Nil -> None

let option_apply v fo = Option.bind fo (function f -> Option.map f v)

let get_note graph subject : Note.t option =
  let id = Rdf.(Triple.Subject.to_term subject |> Term.to_iri) in
  let content =
    Rdf.Graph.objects subject
      (Rdf.Triple.Predicate.of_iri @@ as' "content")
      graph
    |> seq_next
    |> Option.map @@ fun o ->
       Rdf.Triple.Object.map o Rdf.Iri.to_string Rdf.Blank_node.identifier
         Rdf.Literal.canonical
  in
  let lat =
    Rdf.Graph.objects subject (Rdf.Triple.Predicate.of_iri @@ geo "lat") graph
    |> seq_next
    |> Option.map (fun o ->
           Rdf.Triple.Object.map o Rdf.Iri.to_string Rdf.Blank_node.identifier
             Rdf.Literal.canonical)
    |> fun x -> Option.bind x Float.of_string_opt
  in
  let long =
    Rdf.Graph.objects subject (Rdf.Triple.Predicate.of_iri @@ geo "long") graph
    |> seq_next
    |> Option.map (fun o ->
           Rdf.Triple.Object.map o Rdf.Iri.to_string Rdf.Blank_node.identifier
             Rdf.Literal.canonical)
    |> fun x -> Option.bind x Float.of_string_opt
  in
  let geoloc =
    (fun lat long -> Geoloc.make lat long)
    |> Option.some |> option_apply lat |> option_apply long
  in
  (fun id content geoloc : Note.t -> { id; content; geoloc })
  |> Option.some |> option_apply id |> option_apply content
  |> option_apply (Some geoloc)

let get_geoloc graph subject : Geoloc.t option =
  (* Option.bind has wrong argument order... *)
  get_note graph subject |> fun o -> Option.bind o (fun note -> note.geoloc)

let view_note ~send_msg (note : Note.t) =
  El.
    [
      p ~at:At.[ class' @@ Jstr.v "post-content" ] [ txt' note.content ];
      footer
        (List.filter_map
           (fun x -> x)
           [
             Option.map
               (fun (geoloc : Geoloc.t) ->
                 Evr.on_el Ev.click (fun _ -> send_msg @@ `ViewOnMap geoloc)
                 @@ a
                      ~at:At.[ href @@ Jstr.v "#" ]
                      [
                        txt'
                          ((Float.to_string @@ geoloc.latitude)
                          ^ ", " ^ Float.to_string @@ geoloc.longitude);
                      ])
               note.geoloc;
           ]);
    ]

let get_object graph subject =
  Rdf.Graph.objects subject (Rdf.Triple.Predicate.of_iri @@ as' "object") graph
  |> seq_next
  |> Option.map Rdf.Triple.Object.to_term
  |> fun o ->
  Option.bind o (fun o -> Rdf.Term.to_iri o)
  |> Option.map Rdf.Triple.Subject.of_iri

let view_activity ~send_msg graph subject =
  let from =
    Rdf.Graph.objects subject (Rdf.Triple.Predicate.of_iri @@ as' "actor") graph
    |> seq_next
    |> Option.map @@ fun o ->
       Rdf.Triple.Object.map o Rdf.Iri.to_string Rdf.Blank_node.identifier
         Rdf.Literal.canonical
  in
  let published =
    Rdf.Graph.objects subject
      (Rdf.Triple.Predicate.of_iri @@ as' "published")
      graph
    |> seq_next
    |> Option.map @@ fun o ->
       Rdf.Triple.Object.map o Rdf.Iri.to_string Rdf.Blank_node.identifier
         Rdf.Literal.canonical
  in
  let note =
    get_object graph subject |> fun s -> Option.bind s (get_note graph)
  in
  let note_el =
    note |> Option.map (view_note ~send_msg) |> Option.value ~default:[]
  in
  El.(
    li
      [
        article
          (header
             [
               div
                 ~at:At.[ class' @@ Jstr.v "post-from" ]
                 (List.filter_map (fun x -> x) [ Option.map txt' from ]);
               div
                 ~at:At.[ class' @@ Jstr.v "post-date" ]
                 (List.filter_map (fun x -> x) [ Option.map txt' published ]);
             ]
          :: note_el);
      ])
