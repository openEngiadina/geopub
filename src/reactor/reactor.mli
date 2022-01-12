(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(** An [Elm](https://elm-lang.org/) inspired flavor of functional
    reactive programming. *)

module Return : sig
  type ('model, 'cmd) t
  (** The type for return *)

  val singleton : 'model -> ('model, 'cmd) t
  (** Create from model*)

  val command : 'cmd -> ('model, 'cmd) t -> ('model, 'cmd) t
  (** Add a command to a Return *)

  val commands : 'cmd list -> ('model, 'cmd) t -> ('model, 'cmd) t
  (** Add multiple commands to a Return *)

  val map_cmd : ('a -> 'b) -> ('model, 'a) t -> ('model, 'b) t
  (** Map on the cmd *)

  val map : ('a -> 'b) -> ('a, 'msg) t -> ('b, 'msg) t
  (** Map the model*)

  val bind : ('a -> ('b, 'msg) t) -> ('a, 'msg) t -> ('b, 'msg) t

  val run : ('cmd -> unit) -> ('model, 'cmd) t -> 'model
  (** Run the side effects and return the model*)
end

module App : sig
  type ('a, 'model, 'msg) t

  type ('model, 'msg) init = unit -> ('model, 'msg Lwt.t) Return.t
  (** An init function retuns the initial model and side-effects. *)

  type ('a, 'model, 'msg) update =
    stop:('a -> unit) ->
    send_msg:(?step:React.step -> 'msg -> unit) ->
    'model ->
    'msg ->
    ('model, 'msg Lwt.t) Return.t

  val create :
    init:('model, 'msg) init ->
    update:('a, 'model, 'msg) update ->
    ('a, 'model, 'msg) t
  (** [create ~init:init ~update:update] creates an app described by the provided [init] and [update] functions. *)

  val start : ('a, 'model, 'msg) t -> unit
  val model : ('a, 'model, 'msg) t -> 'model React.signal
  val send : ('a, 'model, 'msg) t -> ?step:React.step -> 'msg -> unit
  val result : ('a, 'model, 'msg) t -> ('a, exn) Lwt_result.t
end
