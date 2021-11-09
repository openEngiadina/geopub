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
  type t = { message : Xmpp.Stanza.Message.t; atom : Atom.Entry.t }

  let to_marker post =
    match post.atom.geoloc with
    | Some geoloc ->
        geoloc |> Geoloc.to_latlng |> Leaflet.Marker.create |> Option.some
    | _ -> None
end

type t = Post.t list

type msg = ReceiveMessage of Xmpp.Stanza.Message.t | AddPost of Post.t

let init () = return_nil

let parse_message (message : Xmpp.Stanza.Message.t) =
  let atom_parser =
    Xmlc.Parser.(
      element (ns_pubsub_event "event") (fun _ ->
          element (ns_pubsub_event "items") (fun _ ->
              element (ns_pubsub_event "item") (fun _ -> Atom.Entry.parser))))
  in
  (* Ignore the message if it failes to parse *)
  Lwt_result.catch (Xmlc.parse_trees atom_parser message.payload)
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
      |> Option.map (fun marker -> Leaflet.Marker.add_to marker map)
      |> ignore;
      post :: model |> Return.singleton

let view_compose_form send_msg _client =
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

      send_msg @@ `XmppMsg (Xmppg.PublishPost { title; content }))
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
              li
                [
                  input
                    ~at:
                      At.[ type' @@ Jstr.v "submit"; value @@ Jstr.v "Publish" ]
                    ();
                ];
            ];
        ])

let view_post (post : Post.t) =
  El.(
    li
      [
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
                     (fun from -> txt' @@ Xmpp.Jid.to_string from)
                     post.message.from;
                   Option.some @@ txt' " (";
                   Option.some @@ txt' @@ Ptime.to_rfc3339 post.atom.updated;
                   Option.some @@ txt' " )";
                   Option.map
                     (fun _geoloc -> txt' "Show on map (TODO)")
                     post.atom.geoloc;
                 ]);
            p
              ~at:At.[ class' @@ Jstr.v "post-content" ]
              [ txt' post.atom.content ];
          ];
      ])

let view send_msg xmpp posts =
  match xmpp with
  | L.Loaded xmpp ->
      return
      @@ El.
           [
             Roster.subscriptions_sidebar send_msg None xmpp;
             div
               ~at:At.[ class' @@ Jstr.v "content" ]
               [
                 view_compose_form send_msg xmpp.client;
                 ul
                   ~at:At.[ class' @@ Jstr.v "posts" ]
                   (List.map view_post posts);
               ];
           ]
  | _ -> return_nil
