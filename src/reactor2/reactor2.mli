(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt_react

(** An [Elm](https://elm-lang.org/) inspired flavor of functional
    reactive programming. *)

module Return : sig
  type ('model, 'msg) t
  (** The type for return *)

  val singleton : 'model -> ('model, 'msg) t
  (** Create from model*)

  val command : 'msg Lwt.t -> ('model, 'msg) t -> ('model, 'msg) t
  (** Add a command to a Return *)

  val commands : 'msg Lwt.t list -> ('model, 'msg) t -> ('model, 'msg) t
  (** Add multiple commands to a Return *)

  val map_cmd : ('a -> 'b) -> ('model, 'a) t -> ('model, 'b) t
  (** Map on the cmd *)

  val map : ('a -> 'b) -> ('a, 'msg) t -> ('b, 'msg) t
  (** Map the model*)

  val bind : ('a -> ('b, 'msg) t) -> ('a, 'msg) t -> ('b, 'msg) t
end

module App : sig
  type ('a, 'model, 'msg) t

  type ('model, 'msg) init = unit -> ('model, 'msg) Return.t
  (** An init function retuns the initial model and side-effects. *)

  type ('a, 'model, 'msg) update =
    stop:('a -> unit) -> 'model -> 'msg -> ('model, 'msg) Return.t

  type ('a, 'model, 'msg) subscriptions = 'model -> 'msg E.t

  val create :
    init:('model, 'msg) init ->
    update:('a, 'model, 'msg) update ->
    subscriptions:('a, 'model, 'msg) subscriptions ->
    ('a, 'model, 'msg) t
  (** [create ~init:init ~update:update] creates an app described by the provided [init] and [update] functions. *)

  val model : ('a, 'model, 'msg) t -> 'model React.signal
  val send : ('a, 'model, 'msg) t -> 'msg -> unit
  val result : ('a, 'model, 'msg) t -> ('a, exn) Lwt_result.t
end
