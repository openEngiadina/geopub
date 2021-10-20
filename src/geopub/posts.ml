(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Reactor_brr
open Brr
open Brr_io
open Lwt
open Lwt.Syntax
module L = Loadable

let ns_pubsub_event local = ("http://jabber.org/protocol/pubsub#event", local)

module Post = struct
  type t = { from : Xmpp.Jid.t; atom : Atom.Entry.t }

  let of_contacts contacts =
    let atom_parser =
      Xmlc.Parser.(
        element (ns_pubsub_event "event") (fun _ ->
            element (ns_pubsub_event "items") (fun _ ->
                element (ns_pubsub_event "item") (fun _ -> Atom.Entry.parser))))
    in
    let of_contact jid (contact : Xmppg.contact) =
      contact.messages
      |> Lwt_list.filter_map_s (fun (message : Xmpp.Stanza.Message.t) ->
             (* Ignore the message if it failes to parse *)
             Lwt_result.catch (Xmlc.parse_trees atom_parser message.payload)
             >|= Result.map_error (fun exn ->
                     Console.log [ exn |> Printexc.to_string |> Jstr.v ])
             >|= Result.to_option)
      >|= List.map (fun atom -> { from = jid; atom })
    in
    contacts |> Xmpp.Jid.Map.to_seq |> List.of_seq
    |> Lwt_list.map_s (fun (jid, contact) -> of_contact jid contact)
    >|= List.flatten
end

let compose_form send_msg _client =
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

let post_view (post : Post.t) =
  El.(
    li
      [
        div
          ~at:At.[ class' @@ Jstr.v "post" ]
          [
            h3 ~at:At.[ class' @@ Jstr.v "post-title" ] [ txt' post.atom.title ];
            div
              ~at:At.[ class' @@ Jstr.v "post-meta" ]
              [
                txt' @@ Xmpp.Jid.to_string post.from;
                txt' " (";
                txt' @@ Ptime.to_rfc3339 post.atom.updated;
                txt' " )";
              ];
            p
              ~at:At.[ class' @@ Jstr.v "post-content" ]
              [ txt' post.atom.content ];
          ];
      ])

let view send_msg (model : Xmppg.model L.t) =
  match model with
  | L.Loaded model ->
      let* posts = Post.of_contacts model.contacts in
      return
      @@ El.
           [
             Roster.subscriptions_sidebar send_msg None model;
             div
               ~at:At.[ class' @@ Jstr.v "content" ]
               [
                 compose_form send_msg model.client;
                 ul
                   ~at:At.[ class' @@ Jstr.v "posts" ]
                   (List.map post_view posts);
               ];
           ]
  | _ -> return_nil
