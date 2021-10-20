(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Reactor_brr
open Brr
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
          @@ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' @@ Xmpp.Jid.to_string jid ];
        ])
  in
  El.(
    div
      ~at:At.[ class' @@ Jstr.v "sidebar" ]
      [
        h3 [ txt' "Contacts" ];
        ul
          ~at:At.[ class' @@ Jstr.v "roster" ]
          (Xmpp.Jid.Map.to_seq model.contacts
          |> Seq.map contact_item_el |> List.of_seq);
      ])

let view send_msg jid (model : Xmppg.model L.t) =
  match model with
  | L.Loaded model ->
      return
      @@ El.
           [
             subscriptions_sidebar send_msg (Some jid) model;
             Evr.on_el Ev.click (fun _ -> send_msg @@ `SetRoute Route.Posts)
             @@ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' "Back to posts" ];
             h2 [ txt' @@ Xmpp.Jid.to_string jid ];
           ]
  | _ -> return_nil
