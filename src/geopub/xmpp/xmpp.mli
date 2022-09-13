(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt_react
open Archi_lwt

(** {1 XMPP} *)

module Jid = Xmppl.Jid
module Stanza = Xmppl.Stanza
module Client = Xmppl_websocket.Client

(** {1 Components} *)

module Connection : sig
  (** Handles XMPP connection *)

  type t
  (** Component state *)

  val component : (unit, t) Component.t
  (** Connection component *)

  val client_signal : t -> (Client.t, exn) Loadable.t signal
  (** [client_signal t] returns the underlying XMPP client as signal. *)

  val client : t -> (Client.t, exn) result
  (** [client t] returns the underlying XMPP client. *)

  val login :
    t ->
    ?options:Client.transport_options ->
    password:string ->
    Jid.t ->
    unit Lwt.t

  val login_anonymous_demo : t -> unit Lwt.t
end

type t

val component : (unit, t) Component.t
val connection : t -> Connection.t
