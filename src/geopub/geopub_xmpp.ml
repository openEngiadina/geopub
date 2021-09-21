(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* Lwt *)

open Lwt
open Lwt.Syntax
open Lwt_react

(* Reactor *)

open Reactor
open Reactor_brr

(* Brr *)

open Brr
open Brr_io

(* Loadable helper *)

module L = Loadable

(* XMPP and various XEPs *)

module Client = Xmpp_websocket.Client
module Entity_capabilities = Xmpp_entity_capabilities.Make (Client)
module Roster = Xmpp_roster.Make (Client)
module Pubsub = Xmpp_pubsub.Make (Client)

type contact = {
  roster_item : Roster.Item.t option;
  messages : Xmpp.Stanza.Message.t list;
}

module JidMap = Map.Make (struct
  type t = Xmpp.Jid.t

  let compare a b = String.compare (Xmpp.Jid.to_string a) (Xmpp.Jid.to_string b)
end)

type model = {
  client : Client.t;
  (* Lwt thread that listens for XMPP stanzas and handles them *)
  listener : unit event;
  contacts : contact JidMap.t;
}

type t = model L.t

type msg =
  | NoOp
  | Login of Xmpp.Jid.t * string
  | Authenticated of model
  | Logout
  (* Handle incoming and outgoing (already sent) messages *)
  | ReceiveMsg of Xmpp.Stanza.Message.t
  (* Send a message *)
  | SendMsg of Xmpp.Jid.t * string
  (* Publish a simple post *)
  | PublishPost of string

let jid model = Client.jid model.client |> Xmpp.Jid.bare |> Xmpp.Jid.to_string

let jid_opt = function L.Loaded model -> Option.some @@ jid model | _ -> None

let init () =
  L.Idle |> Return.singleton
  |> Return.command
     @@ Lwt.return
          (Login (Xmpp.Jid.of_string_exn "user@strawberry.local", "pencil"))

let get_roster_contacts client =
  let* roster = Roster.get client in
  roster
  |> List.map (fun (roster_item : Roster.Item.t) ->
         (roster_item.jid, { roster_item = Some roster_item; messages = [] }))
  |> List.to_seq |> JidMap.of_seq |> return

let login ~send_msg jid password =
  let* client =
    Client.create
      { ws_endpoint = Some "ws://localhost:5280/xmpp-websocket" }
      jid ~password
  in
  let* () = Client.connect client in
  let* ec_responder =
    Entity_capabilities.advertise ~category:"client" ~type':"web" ~name:"GeoPub"
      ~node:"https://codeberg.org/openEngiadina/geopub"
      [
        "http://jabber.org/protocol/caps";
        "urn:xmpp:microblog:0";
        "urn:xmpp:microblog:0+notify";
        "http://jabber.org/protocol/geoloc";
        "http://jabber.org/protocol/geoloc+notify";
      ]
      client
  in
  let msg_listener =
    Client.stanzas client
    |> E.map (fun stanza ->
           match stanza with
           | Xmpp.Stanza.Message msg -> send_msg @@ `XmppMsg (ReceiveMsg msg)
           | _ -> ())
  in
  let* contacts = get_roster_contacts client in
  let listener =
    E.merge (fun _ _ -> ()) () [ E.stamp ec_responder (); msg_listener ]
  in
  return @@ `XmppMsg (Authenticated { client; listener; contacts })

let contacts_add_incoming_msg contacts (msg : Xmpp.Stanza.Message.t) =
  match msg.from with
  | Some from ->
      JidMap.update (Xmpp.Jid.bare from)
        (function
          | Some contact ->
              Some { contact with messages = msg :: contact.messages }
          | None -> Some { roster_item = None; messages = [ msg ] })
        contacts
  | None -> contacts

let contacts_add_outgoing_msg contacts (msg : Xmpp.Stanza.Message.t) =
  JidMap.update (Xmpp.Jid.bare msg.to')
    (function
      | Some contact -> Some { contact with messages = msg :: contact.messages }
      | None -> Some { roster_item = None; messages = [ msg ] })
    contacts

let make_outgoing_message client to' message =
  let from = Client.jid client in
  Xmpp.Stanza.Message.make ~to' ~from ~type':"chat"
    Xmpp.Xml.[ make_element ~children:[ make_text message ] (Ns.client "body") ]

let send_xmpp_message client message =
  let* () = Client.send_message client message in
  return @@ `NoOp

let update ~send_msg model msg =
  match (model, msg) with
  (* Initiate authentication *)
  | _, Login (jid, password) ->
      L.Loading |> Return.singleton
      |> Return.command (login ~send_msg jid password)
  (* Authentication succeeded *)
  | _, Authenticated model' -> L.Loaded model' |> Return.singleton
  | _, Logout ->
      (* TODO implement Client.disconnect *)
      L.Idle |> Return.singleton
  (* Handle incoming message *)
  | L.Loaded model, ReceiveMsg msg ->
      L.Loaded
        { model with contacts = contacts_add_incoming_msg model.contacts msg }
      |> Return.singleton
  (* Send a message *)
  | L.Loaded ({ client; _ } as model), SendMsg (to', message) ->
      let message = make_outgoing_message client to' message in
      L.Loaded
        {
          model with
          contacts = contacts_add_outgoing_msg model.contacts message;
        }
      |> Return.singleton
      |> Return.command (send_xmpp_message client message)
  | L.Loaded model, PublishPost post_content ->
      let atom_entry = Atom.Entry.make post_content in
      let jid = Client.jid model.client in
      let item =
        Xmpp.Xml.make_element
          ~children:[ Atom.Entry.to_xml atom_entry ]
          (Pubsub.Ns.pubsub "item")
      in
      L.Loaded model |> Return.singleton
      |> Return.command
           ( Pubsub.publish ~to':jid ~node:"urn:xmpp:microblog:0" model.client
               (Option.some @@ item)
           >|= fun _ -> `NoOp )
  | _, _ -> model |> Return.singleton

(* View *)

let login_view send_msg =
  let login_form =
    El.(
      form
        [
          (* JID *)
          label ~at:At.[ for' @@ Jstr.v "jid" ] [ txt' "JID" ];
          input
            ~at:
              At.
                [
                  id @@ Jstr.v "jid";
                  name @@ Jstr.v "jid";
                  type' @@ Jstr.v "text";
                ]
            ();
          (* Password *)
          label ~at:At.[ for' @@ Jstr.v "password" ] [ txt' "Password" ];
          input
            ~at:
              At.
                [
                  id @@ Jstr.v "password";
                  name @@ Jstr.v "password";
                  type' @@ Jstr.v "password";
                ]
            ();
          (* Submit *)
          input
            ~at:
              At.
                [
                  id @@ Jstr.v "submit";
                  type' @@ Jstr.v "submit";
                  value @@ Jstr.v "Login";
                ]
            ();
        ])
  in
  El.
    [
      h1 [ txt' "Login" ];
      p [ txt' "You may login using any XMPP account." ];
      Evr.on_el ~default:false Form.Ev.submit
        (fun ev ->
          let form_data =
            Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
          in

          let jid_value =
            Form.Data.find form_data (Jstr.v "jid") |> Option.get
          in
          let password_value =
            Form.Data.find form_data (Jstr.v "password") |> Option.get
          in

          let jid =
            match jid_value with
            | `String js -> Jstr.to_string js |> Xmpp.Jid.of_string_exn
            | _ -> failwith "We need better error handling"
          in

          let password =
            match password_value with
            | `String js -> Jstr.to_string js
            | _ -> failwith "We need better error handling"
          in

          send_msg @@ `XmppMsg (Login (jid, password)))
        login_form;
    ]

let contacts_sidebar send_msg model =
  let contact_item_el (jid, _contact) =
    El.(
      li
        ~at:At.[ class' @@ Jstr.v "roster-item" ]
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

let chat_view send_msg model selected_jid =
  let message_body (message : Xmpp.Stanza.Message.t) =
    let signals =
      message.payload |> List.map Xmpp.Xml.tree_to_signals |> List.flatten
    in
    let parser =
      Xmpp.Xml.Parser.(
        many
          (choice
             [
               element (Xmpp.Xml.Ns.client "body") (fun _attrs ->
                   text >>| String.concat "" >>| Option.some);
               (ignore_element >>| fun _ -> None);
             ])
        >>| List.filter_map (fun x -> x)
        >>| String.concat "")
    in
    signals |> Lwt_stream.of_list |> Xmpp.Xml.Parser.parse_stream parser
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

        send_msg @@ `XmppMsg (SendMsg (selected_jid, message)))
      El.(
        form
          ~at:At.[ class' @@ Jstr.v "chat-compose" ]
          [
            input ~at:At.[ type' @@ Jstr.v "text"; name @@ Jstr.v "message" ] ();
            input ~at:At.[ type' @@ Jstr.v "submit"; value @@ Jstr.v "Send" ] ();
          ])
  in

  let contact_chat_view model selected_jid contact =
    let* messages = Lwt_list.filter_map_s message_item contact.messages in
    return
    @@ El.
         [
           contacts_sidebar send_msg model;
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
            |> Option.value ~default:{ roster_item = None; messages = [] }
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
            @@ login_view send_msg;
          ]

let account_view send_msg = function
  | L.Idle ->
      El.
        [
          div ~at:At.[ class' @@ Jstr.v "text-content" ] @@ login_view send_msg;
        ]
  | L.Loading ->
      El.[ div ~at:At.[ class' @@ Jstr.v "text-content" ] [ txt' "Loading" ] ]
  | L.Loaded model ->
      El.
        [
          div
            ~at:At.[ class' @@ Jstr.v "text-content" ]
            [
              h1 [ txt' "" ];
              p [ txt' @@ "Authenticated as " ^ jid model ];
              p
                [
                  Evr.on_el Ev.click (fun _ -> send_msg @@ `XmppMsg Logout)
                  @@ button [ txt' "Logout" ];
                ];
            ];
        ]
