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

let ns_pubsub_event local = ("http://jabber.org/protocol/pubsub#event", local)

let atoms_from_contacts contacts =
  let parser =
    Xmpp.Xml.Parser.(
      element (ns_pubsub_event "event") (fun _ ->
          element (ns_pubsub_event "items") (fun _ ->
              element (ns_pubsub_event "item") (fun _ -> Atom.Entry.parser))))
  in
  let from_contact (contact : Xmppg.contact) =
    contact.messages
    |> Lwt_list.filter_map_s (fun (message : Xmpp.Stanza.Message.t) ->
           (* Ignore the message if it failes to parse *)
           Lwt_result.catch (Xmpp.Xml.parse_trees parser message.payload)
           >|= Result.to_option)
  in
  contacts |> JidMap.to_seq |> Seq.map snd |> List.of_seq
  |> Lwt_list.map_s from_contact
  >|= List.flatten

let atom_entry_view (entry : Atom.Entry.t) =
  El.(div ~at:At.[ class' @@ Jstr.v "atom-entry" ] [ txt' entry.title ])

let view send_msg (model : Xmppg.model L.t) =
  match model with
  | L.Loaded model ->
      let* atoms = atoms_from_contacts model.contacts in
      return
      @@ El.
           [
             div
               ~at:At.[ class' @@ Jstr.v "text-content" ]
               [
                 compose_form send_msg model.client;
                 ul
                   ~at:At.[ class' @@ Jstr.v "atom-entry" ]
                   (List.map atom_entry_view atoms);
               ];
           ]
  | _ -> return_nil
