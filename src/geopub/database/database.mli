(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Archi_lwt
open Lwt_react

(** {1 Component} *)

type t

val component : (unit, t) Component.t

(** {1 Insert data } *)

val add_graph : t -> Rdf.Graph.t -> unit Lwt.t

(** {1 Query} *)

type transaction
(** {2 Transactions} *)

val read_only : t -> transaction

(** {2 Datalog} *)

module Datalog = Datalog

val query :
  t -> Datalog.query -> (transaction * Datalog.Tuple.Set.t) signal Lwt.t
(** [query databse query] runs the Datalog query [query] and returns a signal carrying the result tuples. *)

(** {2 RDF queries} *)

val deref_triple :
  t -> transaction -> Datalog.Constant.t list -> Rdf.Triple.t option Lwt.t

val query_rdf : t -> Datalog.query -> Rdf.Graph.t signal Lwt.t
val description : t -> Rdf.Iri.t -> Rdf.Description.t signal Lwt.t
(* [description t iri] returns a signal carrying a RDF description for subject [iri]. *)

val functional_property :
  t ->
  Rdf.Triple.Subject.t ->
  Rdf.Triple.Predicate.t ->
  Rdf.Triple.Object.t option signal Lwt.t
