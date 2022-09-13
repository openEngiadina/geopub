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

let start () =
  let* db = Store.init () in

  let* triple_count = Store.triple_count db in
  Log.debug (fun m -> m "Triples in database: %d" triple_count);

  (* Load some vocabularies if db is empty *)
  (* let* () =
   *   if triple_count = 0 then
   *     let* () =
   *       Vocabs.vocabs
   *       |> Lwt_list.iter_s (fun vocab ->
   *              Log.debug (fun m -> m "Loading vocabulary %s" vocab);
   *              let* graph = Vocabs.fetch_vocab vocab in
   *              Store.add_graph db graph)
   *     in
   *     Sample_data.sample_data
   *     |> Lwt_list.iter_s (fun file ->
   *            Log.debug (fun m -> m "Loading sample data %s" file);
   *            let* graph = Sample_data.fetch file in
   *            Store.add_graph db graph)
   *   else return_unit
   * in *)
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

module Datalog = Datalog

let query db q =
  (* start an initial read only transaction *)
  let init_tx = read_only db in

  (* run an initial datalog query *)
  let* init_state, init_tuples =
    Datalog.(
      query_with_state ~database:(edb init_tx)
        ~state:(init geopub_datalog_program)
        q)
  in

  (* update tuples with incremental evaluation when Store.on_update fires *)
  let update_e =
    E.map
      (fun () (state, _tx, _tuples) ->
        let tx = read_only db in
        let* state', tuples' =
          Datalog.(query_with_state ~database:(edb tx) ~state q)
        in

        return (state', tx, tuples'))
      Store.on_update
  in

  (* accumulate signal *)
  S.accum_s update_e (init_state, init_tx, init_tuples)
  |> S.map (fun (_state, tx, tuples) -> (tx, tuples))
  |> return

let deref_triple db tx = function
  | Datalog.[ Constant.Rdf s_id; Constant.Rdf p_id; Constant.Rdf o_id ] ->
      Store.Triples.deref db ~tx [ s_id; p_id; o_id ]
  | _ -> return_none

let query_rdf db q =
  query db q
  >>= S.map_s ~eq:Rdf.Graph.equal (fun (tx, tuples) ->
          Datalog.Tuple.Set.to_seq tuples
          |> Lwt_seq.of_seq
          |> Lwt_seq.filter_map_s (deref_triple db tx)
          |> Lwt_seq.fold_left
               (fun graph triple -> Rdf.Graph.add triple graph)
               Rdf.Graph.empty)

let term_lookup db term =
  let* id_opt = Store.Dictionary.lookup db ~tx:(read_only db) term in

  let updates =
    E.map
      (fun () id_opt ->
        match id_opt with
        | Some id -> return_some id
        | None -> Store.Dictionary.lookup db ~tx:(read_only db) term)
      Store.on_update
  in

  return @@ S.accum_s ~eq:( = ) updates id_opt

let description db iri =
  let* q_opt_s =
    term_lookup db (Rdf.Term.of_iri iri)
    >|= S.map
          (Option.map (fun s_id ->
               Datalog.(
                 Atom.make "triple"
                   Term.
                     [
                       make_constant @@ Constant.Rdf s_id;
                       make_variable "p";
                       make_variable "o";
                     ])))
  in

  let s = Rdf.Triple.Subject.of_iri iri in

  S.bind_s ~eq:Rdf.Description.equal q_opt_s (function
    | Some q -> query_rdf db q >|= S.map (Rdf.Graph.description s)
    | None -> return @@ S.const (Rdf.Description.empty s))

let functional_property db subject predicate =
  let* s_id_s = term_lookup db (Rdf.Triple.Subject.to_term subject) in
  let* p_id_s = term_lookup db (Rdf.Triple.Predicate.to_term predicate) in

  let q_opt_s =
    S.l2
      (fun s_id_opt p_id_opt ->
        match (s_id_opt, p_id_opt) with
        | Some s_id, Some p_id ->
            Option.some
            @@ Datalog.(
                 Atom.make "triple"
                   Term.
                     [
                       make_constant @@ Constant.Rdf s_id;
                       make_constant @@ Constant.Rdf p_id;
                       make_variable "o";
                     ])
        | _ -> None)
      s_id_s p_id_s
  in

  S.bind_s ~eq:( = ) q_opt_s (function
    | Some q ->
        query_rdf db q
        >|= S.map (Rdf.Graph.functional_property subject predicate)
    | None -> return @@ S.const None)
