(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: ISC
 *)

module Cursor : sig
  type t

  (** {1 Properties} *)

  val value : t -> Jv.t
  val key : t -> Jv.t
  val primary_key : t -> Jv.t

  (** {1 Iterators} *)

  val to_seq : t -> t Lwt_seq.t
  (** [to_seq cursor] returns a sequence of the same cursor that is advanced to the next position at every iteration. *)

  val opt_lwt_to_seq : t option Lwt.t -> t Lwt_seq.t
end

module Index : sig
  type t

  (** {1 Properties} *)

  val key_path : t -> Jv.t

  (** {1 Methods} *)

  val count : t -> Jv.t -> int Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex/count *)

  val get : t -> Jv.t -> Jv.t option Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex/get *)

  val get_all : t -> ?count:int -> Jv.t -> Jv.t list Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex/getAll *)

  val get_key : t -> Jv.t -> Jv.t Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex/getKey *)

  val open_cursor : t -> Jv.t -> Cursor.t option Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex/openCursor *)
end

module ObjectStore : sig
  type t

  val add : t -> ?key:Jv.t -> Jv.t -> Jv.t Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore/add *)

  val put : t -> ?key:Jv.t -> Jv.t -> Jv.t Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore/put *)

  val get : t -> Jv.t -> Jv.t option Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore/get *)

  val get_all : t -> ?count:int -> Jv.t -> Jv.t list Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore/getAll *)

  val open_cursor : t -> Jv.t -> Cursor.t option Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore/openCursor *)

  val count : t -> Jv.t -> int Lwt.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore/count *)

  val index : t -> Jstr.t -> Index.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore/index *)

  (** {1 On Version Change} *)

  val create_index :
    t -> key_path:Jv.t -> ?object_parameters:Jv.t -> Jstr.t -> Index.t
  (** https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore/createIndex *)
end

module Database : sig
  type t

  val open' :
    ?version:int -> ?on_version_change:(t -> unit) -> Jstr.t -> t Lwt.t

  (** {1 On Version Change} *)

  val create_object_store : t -> ?options:Jv.t -> Jstr.t -> ObjectStore.t
  val delete_object_store : t -> Jstr.t -> unit

  (** {1 Properties} *)
  val object_store_names : t -> Jstr.t list
  (** [object_store_names db] returns a list of names of the object
  stores currently in the connected database.

  @see https://developer.mozilla.org/en-US/docs/Web/API/IDBDatabase/objectStoreNames *)

  val delete : Jstr.t -> unit Lwt.t
  (** {1 Methods} *)

  val close : t -> unit
end

module Transaction : sig
  type t
  type mode = ReadOnly | ReadWrite

  val create : Database.t -> ?mode:mode -> ?options:Jv.t -> Jstr.t list -> t
  val object_store : t -> Jstr.t -> ObjectStore.t
  val commit : t -> unit
  val abort : t -> unit
end
