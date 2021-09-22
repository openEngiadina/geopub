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
module JidMap = Xmppg.JidMap

let ns_pubsub_event local = ("http://jabber.org/protocol/pubsub#event", local)

module Post = struct
  type t = { from : Xmpp.Jid.t; atom : Atom.Entry.t }

  let of_contacts contacts =
    let atom_parser =
      Xmpp.Xml.Parser.(
        element (ns_pubsub_event "event") (fun _ ->
            element (ns_pubsub_event "items") (fun _ ->
                element (ns_pubsub_event "item") (fun _ -> Atom.Entry.parser))))
    in
    let of_contact jid (contact : Xmppg.contact) =
      contact.messages
      |> Lwt_list.filter_map_s (fun (message : Xmpp.Stanza.Message.t) ->
             (* Ignore the message if it failes to parse *)
             Lwt_result.catch (Xmpp.Xml.parse_trees atom_parser message.payload)
             >|= Result.to_option)
      >|= List.map (fun atom -> { from = jid; atom })
    in
    contacts |> JidMap.to_seq |> List.of_seq
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

      let post_content_value =
        Form.Data.find form_data (Jstr.v "post-content") |> Option.get
      in

      let post_content =
        match post_content_value with
        | `String js -> Jstr.to_string js
        | _ -> failwith "We need better error handling"
      in

      send_msg @@ `XmppMsg (Xmppg.PublishPost post_content))
    El.(
      form
        ~at:At.[ class' @@ Jstr.v "post-compose" ]
        [
          input
            ~at:At.[ type' @@ Jstr.v "text"; name @@ Jstr.v "post-content" ]
            ();
          input
            ~at:At.[ type' @@ Jstr.v "submit"; value @@ Jstr.v "Publish" ]
            ();
        ])

let post_view (post : Post.t) =
  let atom_view (entry : Atom.Entry.t) =
    El.(p ~at:At.[ class' @@ Jstr.v "atom-entry" ] [ txt' entry.title ])
  in
  El.(
    li
      ~at:At.[ class' @@ Jstr.v "post-item" ]
      [
        div
          ~at:At.[ class' @@ Jstr.v "post-jid" ]
          [ txt' @@ Xmpp.Jid.to_string post.from ^ ":" ];
        atom_view post.atom;
      ])

let view send_msg (model : Xmppg.model L.t) =
  match model with
  | L.Loaded model ->
      let* posts = Post.of_contacts model.contacts in
      return
      @@ El.
           [
             div
               ~at:At.[ class' @@ Jstr.v "text-content" ]
               [
                 compose_form send_msg model.client;
                 ul
                   ~at:At.[ class' @@ Jstr.v "posts" ]
                   (List.map post_view posts);
               ];
           ]
  | _ -> return_nil