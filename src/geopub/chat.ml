(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Brr
open Brr_io
open Reactor_brr
module L = Loadable
module JidMap = Xmppg.JidMap

let contacts_sidebar send_msg ?selected_jid (model : Xmppg.model) =
  let contact_item_el (jid, _contact) =
    El.(
      li
        ~at:
          (List.filter_map
             (fun x -> x)
             At.
               [
                 Option.some @@ class' @@ Jstr.v "roster-item";
                 Option.bind selected_jid (fun selected_jid ->
                     if selected_jid = jid then
                       Option.some @@ class' @@ Jstr.v "roster-item-selected"
                     else None);
               ])
        [
          Evr.on_el Ev.click (fun _ ->
              send_msg @@ `SetRoute (Route.Chat (Some jid)))
          @@ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' @@ Xmpp.Jid.to_string jid ];
        ])
  in
  El.(
    div
      ~at:At.[ class' @@ Jstr.v "sidebar" ]
      [
        ul
          ~at:At.[ class' @@ Jstr.v "roster" ]
          (JidMap.to_seq model.contacts
          |> Seq.map contact_item_el |> List.of_seq);
      ])

let view send_msg (model : Xmppg.model L.t) selected_jid =
  let message_body (message : Xmpp.Stanza.Message.t) =
    let parser =
      Xmlc.Parser.(
        many
          (choice
             [
               element (Xmpp.Ns.client "body") (fun _attrs ->
                   text >>| String.concat "" >>| Option.some);
               (ignore_element >>| fun _ -> None);
             ])
        >>| List.filter_map (fun x -> x)
        >>| String.concat "")
    in
    Xmlc.parse_trees parser message.payload
  in

  let message_item (message : Xmpp.Stanza.Message.t) =
    match message.type' with
    | Some "chat" ->
        let from =
          Option.map
            (fun from -> Xmpp.Jid.(to_string @@ bare from))
            message.from
          |> Option.value ~default:""
        in
        let* message_body = message_body message in
        Lwt.return_some
        @@ El.(
             li
               ~at:At.[ class' @@ Jstr.v "message" ]
               [
                 div
                   ~at:At.[ class' @@ Jstr.v "message-from" ]
                   [ txt' from; txt' ":" ];
                 p
                   ~at:At.[ class' @@ Jstr.v "message-body" ]
                   [ txt' message_body ];
               ])
    | _ -> Lwt.return_none
  in

  let compose_form selected_jid =
    Evr.on_el ~default:false Form.Ev.submit
      (fun ev ->
        let form_data =
          Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
        in

        let message_value =
          Form.Data.find form_data (Jstr.v "message") |> Option.get
        in

        let message =
          match message_value with
          | `String js -> Jstr.to_string js
          | _ -> failwith "We need better error handling"
        in

        send_msg @@ `XmppMsg (Xmppg.SendMsg (selected_jid, message)))
      El.(
        form
          ~at:At.[ class' @@ Jstr.v "chat-compose" ]
          [
            input ~at:At.[ type' @@ Jstr.v "text"; name @@ Jstr.v "message" ] ();
            input ~at:At.[ type' @@ Jstr.v "submit"; value @@ Jstr.v "Send" ] ();
          ])
  in

  let contact_chat_view model selected_jid (contact : Xmppg.contact) =
    let* messages = Lwt_list.filter_map_s message_item contact.messages in
    return
    @@ El.
         [
           contacts_sidebar ~selected_jid send_msg model;
           div
             ~at:At.[ class' @@ Jstr.v "chat" ]
             [
               ul ~at:At.[ class' @@ Jstr.v "messages" ] @@ List.rev messages;
               compose_form selected_jid;
             ];
         ]
  in

  match model with
  | L.Loaded model -> (
      match selected_jid with
      | Some selected_jid ->
          let contact =
            JidMap.find_opt selected_jid model.contacts
            |> Option.value
                 ~default:
                   ({ roster_item = None; messages = [] } : Xmppg.contact)
          in
          contact_chat_view model selected_jid contact
      | None ->
          return
            El.
              [
                contacts_sidebar send_msg model; div [ txt' "Select a contact" ];
              ])
  | _ ->
      return
        El.
          [
            div ~at:At.[ class' @@ Jstr.v "text-content" ]
            @@ [ txt' "Not logged in" ];
          ]
