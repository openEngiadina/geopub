(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Reactor_brr
open Brr
open Brr_io
open Lwt
module L = Loadable

let subscriptions_sidebar send_msg selected_jid (model : Xmppg.model) =
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
          Evr.on_el Ev.click (fun _ -> send_msg @@ `SetRoute (Route.Roster jid))
          @@ a
               ~at:At.[ href @@ Jstr.v "#" ]
               [ txt' @@ Xmppg.contact_display_name model jid ];
        ])
  in
  El.(
    div
      ~at:At.[ class' @@ Jstr.v "sidebar" ]
      [
        h3 [ txt' "Contacts" ];
        ul
          ~at:At.[ class' @@ Jstr.v "roster" ]
          (Xmppl.Jid.Map.to_seq model.roster
          |> Seq.map contact_item_el |> List.of_seq);
        Evr.on_el Ev.click (fun _ -> send_msg @@ `SetRoute Route.AddContact)
        @@ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' "Add contact" ];
      ])

let contact_subscription_from (model : Xmppg.model) jid =
  Xmppl.Jid.Map.find_opt jid model.roster
  |> Option.map (fun (roster_item : Xmppg.Roster.Item.t) ->
         let subscription =
           roster_item.subscription |> Option.value ~default:"none"
         in
         (* contact is pre-approved *)
         roster_item.approved |> Option.value ~default:false
         (* contact is subscribed *)
         || subscription = "from"
         || subscription = "both")
  |> Option.value ~default:false

let contact_subscription_to (model : Xmppg.model) jid =
  Xmppl.Jid.Map.find_opt jid model.roster
  |> Option.map (fun (roster_item : Xmppg.Roster.Item.t) ->
         let subscription =
           roster_item.subscription |> Option.value ~default:"none"
         in
         let ask = roster_item.ask |> Option.value ~default:"none" in
         subscription = "to" || subscription = "both" || ask = "subscribe")
  |> Option.value ~default:false

let view send_msg jid (model : Xmppg.model L.t) =
  match model with
  | L.Loaded model ->
      return
      @@ El.
           [
             subscriptions_sidebar send_msg (Some jid) model;
             div
               [
                 Evr.on_el Ev.click (fun _ ->
                     send_msg @@ `SetRoute (Route.Posts None))
                 @@ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' "Back to posts" ];
                 h2 [ txt' @@ Xmppg.contact_display_name model jid ];
                 dl
                   [
                     dt [ txt' "JID" ];
                     dd [ txt' @@ Xmppl.Jid.to_string jid ];
                     dt [ txt' "Send posts and presence updates" ];
                     dd
                       [
                         Evr.on_el Ev.click (fun _ ->
                             if contact_subscription_from model jid then
                               send_msg
                               @@ `XmppMsg (Xmppg.PresenceDenySubscription jid)
                             else
                               send_msg
                               @@ `XmppMsg
                                    (Xmppg.PresenceApproveSubscription jid))
                         @@ input
                              ~at:
                                At.(
                                  List.filter_map
                                    (fun x -> x)
                                    [
                                      Some (type' @@ Jstr.v "checkbox");
                                      (if contact_subscription_from model jid
                                      then Some checked
                                      else None);
                                    ])
                              ();
                       ];
                     dt [ txt' "Receive posts and presence updates" ];
                     dd
                       [
                         Evr.on_el Ev.click (fun _ ->
                             if contact_subscription_to model jid then
                               send_msg
                               @@ `XmppMsg (Xmppg.PresenceUnsubscribe jid)
                             else
                               send_msg
                               @@ `XmppMsg (Xmppg.PresenceSubscribe jid))
                         @@ input
                              ~at:
                                At.(
                                  List.filter_map
                                    (fun x -> x)
                                    [
                                      Some (type' @@ Jstr.v "checkbox");
                                      (if contact_subscription_to model jid then
                                       Some checked
                                      else None);
                                    ])
                              ();
                       ];
                   ];
               ];
           ]
  | _ -> return_nil

let view_add_contact send_msg (model : Xmppg.model L.t) =
  match model with
  | L.Loaded model ->
      return
        El.
          [
            subscriptions_sidebar send_msg None model;
            div
              [
                Evr.on_el Ev.click (fun _ ->
                    send_msg @@ `SetRoute (Route.Posts None))
                @@ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' "Back to posts" ];
                Evr.on_el ~default:false Form.Ev.submit (fun ev ->
                    let form_data =
                      Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv
                      @@ Ev.target ev
                    in

                    let jid_value =
                      Form.Data.find form_data (Jstr.v "jid") |> Option.get
                    in

                    let jid =
                      match jid_value with
                      | `String js ->
                          Jstr.to_string js |> Xmppl.Jid.of_string_exn
                      | _ -> failwith "Invalid JID given while adding contact"
                    in

                    send_msg @@ `XmppMsg (Xmppg.AddContact jid))
                @@ form
                     [
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
                       input
                         ~at:
                           At.
                             [
                               id @@ Jstr.v "add";
                               type' @@ Jstr.v "submit";
                               value @@ Jstr.v "Add contact";
                             ]
                         ();
                     ];
              ];
          ]
  | _ -> return_nil
