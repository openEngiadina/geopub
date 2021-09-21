(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

type contact = { jid : Xmpp.Jid.t; messages : Xmpp.Stanza.Message.t list }

let message_item (message : Xmpp.Stanza.Message.t) =
  El.(li [ txt' message.type' ])

let view _send_msg contact =
  El.(
    ul
      ~at:At.[ class' @@ Jstr.v "chat" ]
      (List.map message_item contact.messages))
