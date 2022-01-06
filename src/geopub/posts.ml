(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Reactor
open Reactor_brr
open Brr
open Brr_io
open Lwt
module L = Loadable

let ns_pubsub_event local = ("http://jabber.org/protocol/pubsub#event", local)

module Post = struct
  type t = { message : Xmppl.Stanza.Message.t; atom : Atom.Entry.t }

  let to_marker post =
    match post.atom.geoloc with
    | Some geoloc ->
        geoloc |> Geoloc.to_latlng |> Leaflet.Marker.create |> Option.some
    | _ -> None

  let view ~send_msg post =
    El.(
      div
        ~at:At.[ class' @@ Jstr.v "post" ]
        [
          h3 ~at:At.[ class' @@ Jstr.v "post-title" ] [ txt' post.atom.title ];
          div
            ~at:At.[ class' @@ Jstr.v "post-meta" ]
            (List.filter_map
               (fun x -> x)
               [
                 Option.map
                   (fun from -> txt' @@ Xmppl.Jid.to_string from)
                   post.message.from;
                 Option.some @@ br ();
                 Option.some @@ txt' @@ Ptime.to_rfc3339 post.atom.updated;
                 Option.some @@ br ();
                 Option.map
                   (fun (geoloc : Geoloc.t) ->
                     Evr.on_el Ev.click (fun _ ->
                         send_msg
                         @@ `MapMsg (Mapg.SetView (Geoloc.to_latlng geoloc));
                         send_msg @@ `SetRoute Route.Map)
                     @@ a
                          ~at:At.[ href @@ Jstr.v "#" ]
                          [
                            txt'
                              ((Float.to_string @@ geoloc.latitude)
                              ^ ", " ^ Float.to_string @@ geoloc.longitude);
                          ])
                   post.atom.geoloc;
               ]);
          p
            ~at:At.[ class' @@ Jstr.v "post-content" ]
            [ txt' post.atom.content ];
        ])
end

type t = Post.t list
type msg = ReceiveMessage of Xmppl.Stanza.Message.t | AddPost of Post.t

let init () = return_nil

let parse_message (message : Xmppl.Stanza.Message.t) =
  let atom_parser =
    Xmlc.Parser.(
      element (ns_pubsub_event "event") (fun _ ->
          element (ns_pubsub_event "items") (fun _ ->
              element (ns_pubsub_event "item") (fun _ -> Atom.Entry.parser))))
  in
  (* Ignore the message if it failes to parse *)
  Lwt_result.catch
    (Xmlc.Tree.parse_trees atom_parser (List.to_seq message.payload))
  >|= Result.map (fun atom -> `PostsMsg (AddPost { message; atom }))
  >|= Result.value ~default:`NoOp

let update ~send_msg map model msg =
  ignore send_msg;
  match msg with
  | ReceiveMessage message ->
      model |> Return.singleton |> Return.command (parse_message message)
  | AddPost post ->
      (* Add marker to map *)
      (* TODO this is a horrible mix of imperative and declarative code ... needs to be fixed *)
      post |> Post.to_marker
      |> Option.map (Leaflet.Marker.bind_popup (Post.view ~send_msg post))
      |> Option.map (fun marker ->
             Option.map (Leaflet.Marker.add_to marker) map)
      |> ignore;
      post :: model |> Return.singleton

let view_compose_form send_msg latlng =
  ignore send_msg;
  Evr.on_el ~default:false Form.Ev.submit
    (fun ev ->
      let form_data =
        Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
      in

      let post_title_value =
        Form.Data.find form_data (Jstr.v "post-title") |> Option.get
      in

      let title =
        match post_title_value with
        | `String js -> Jstr.to_string js
        | _ -> failwith "We need better error handling"
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
          (fun latlng -> Leaflet.LatLng.(Geoloc.make (lat latlng) (lng latlng)))
          latlng
      in

      let make_atom author ~id =
        Atom.Entry.make ~title ~content ?geoloc ~authors:[ author ] ~id
          ~updated:(Ptime_clock.now ()) ()
      in

      send_msg @@ `XmppMsg (Xmppg.PublishPost make_atom))
    El.(
      form
        ~at:At.[ class' @@ Jstr.v "post-compose" ]
        [
          ul
            [
              li
                [
                  label ~at:At.[ for' @@ Jstr.v "post-title" ] [ txt' "title" ];
                  input
                    ~at:
                      At.
                        [
                          id @@ Jstr.v "post-title";
                          type' @@ Jstr.v "text";
                          name @@ Jstr.v "post-title";
                        ]
                    ();
                ];
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
                      At.[ type' @@ Jstr.v "submit"; value @@ Jstr.v "Publish" ]
                    ();
                ];
            ];
        ])

let view send_msg latlng xmpp posts =
  match xmpp with
  | L.Loaded xmpp ->
      return
      @@ El.
           [
             Roster.subscriptions_sidebar send_msg None xmpp;
             div
               ~at:At.[ class' @@ Jstr.v "content" ]
               [
                 view_compose_form send_msg latlng;
                 ul
                   ~at:At.[ class' @@ Jstr.v "posts" ]
                   (List.map
                      (fun post -> li [ Post.view ~send_msg post ])
                      posts);
               ];
           ]
  | _ -> return_nil
