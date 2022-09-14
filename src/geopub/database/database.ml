(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Lwt_react

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Database"

module Log = (val Logs.src_log src : Logs.LOG)

(* Type of database *)
type t = Indexeddb.Database.t

module Store = Store

(* Component *)

open Archi_lwt

let start _ =
  let* db = Store.init () in

  let* triple_count = Store.triple_count db in
  Log.debug (fun m -> m "Triples in database: %d" triple_count);

  Log.info (fun m -> m "Database started.");
  return_ok db

let stop _db =
  Log.info (fun m -> m "Database stopped.");
  return_unit

let component = Component.make ~start ~stop

(* Query *)

(* Transactions *)

type transaction = Indexeddb.Transaction.t

let read_only db = Store.ro_tx db

(* Inserting data *)

let add_graph = Store.add_graph

(* Getting data *)

module EDatalog = Datalog
module Datalog = Dictionary_datalog

let query db q =
  (* start an initial read only transaction *)
  let init_tx = read_only db in

  (* FIXME: There is a logical bug here. The query is looked up only
     once when initializing the signal. If it can not be translated to
     identifiers the query will always fail even if later the terms are
     inserted that allow the query to be properly looked up. *)

  (* translate RDF terms to dictionary identifiers *)
  let* eq = Datalog.Dictionary.lookup_atom db ~tx:init_tx q in

  (* run an initial datalog query *)
  let* init_state, init_etuples =
    EDatalog.(
      query_with_state ~database:(edb init_tx)
        ~state:(init geopub_datalog_program)
        eq)
  in

  let* init_tuples =
    Datalog.Dictionary.get_tuple_set db ~tx:init_tx init_etuples
  in

  (* update tuples with incremental evaluation when Store.on_update fires *)
  let update_e =
    E.map
      (fun () (state, _tx, _tuples) ->
        let tx = read_only db in
        let* state', etuples' =
          EDatalog.(query_with_state ~database:(edb tx) ~state eq)
        in

        let* tuples' = Datalog.Dictionary.get_tuple_set db ~tx etuples' in

        return (state', tx, tuples'))
      Store.on_update
  in

  (* accumulate signal *)
  S.accum_s update_e (init_state, init_tx, init_tuples)
  |> S.map
       ~eq:(fun (_, a) (_, b) -> Datalog.Tuple.Set.equal a b)
       (fun (_state, tx, tuples) -> (tx, tuples))
  |> return

let triple_of_tuple = function
  | Datalog.[ Constant.Rdf s; Constant.Rdf p; Constant.Rdf o ] ->
      let subject_opt =
        Rdf.(
          Term.map
            (fun iri -> Some (Triple.Subject.of_iri iri))
            (fun bnode -> Some (Triple.Subject.of_blank_node bnode))
            (fun _ -> None)
            s)
      in

      let predicate_opt =
        Rdf.(
          Term.map
            (fun iri -> Option.some @@ Triple.Predicate.of_iri iri)
            (fun _ -> None)
            (fun _ -> None)
            p)
      in

      Option.(
        map Rdf.Triple.make subject_opt
        |> (fun opt -> bind opt (fun f -> map f predicate_opt))
        |> map (fun f -> f @@ Rdf.Triple.Object.of_term o))
  | _ -> None

let query_rdf db q =
  query db q
  >|= S.map ~eq:Rdf.Graph.equal (fun (_tx, tuples) ->
          Datalog.Tuple.Set.to_seq tuples
          |> Seq.filter_map triple_of_tuple
          |> Rdf.Graph.of_triples)

let description db term =
  let q =
    Datalog.(
      Atom.make "triple"
        Term.
          [
            make_constant @@ Constant.Rdf term;
            make_variable "p";
            make_variable "o";
          ])
  in

  let s =
    Rdf.Term.map Rdf.Triple.Subject.of_iri Rdf.Triple.Subject.of_blank_node
      (fun _ -> failwith "term can not be an object")
      term
  in

  query_rdf db q >|= S.map (Rdf.Graph.description s)

let get_description db term = description db term >|= S.value

let functional_property db subject predicate =
  let q =
    Datalog.(
      Atom.make "triple"
        Term.
          [
            make_constant @@ Constant.Rdf (Rdf.Triple.Subject.to_term subject);
            make_constant
            @@ Constant.Rdf (Rdf.Triple.Predicate.to_term predicate);
            make_variable "o";
          ])
  in

  query_rdf db q >|= S.map (Rdf.Graph.functional_property subject predicate)

let get_functional_property db subject predicate =
  functional_property db subject predicate >|= S.value

(* Deleting Database *)

let delete = Store.delete
