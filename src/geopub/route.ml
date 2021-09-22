(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

type t = About | Map | Posts | Chat of Xmpp.Jid.t option | Account

type action = [ `SetRoute of t ]