(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Database"

module Log = (val Logs.src_log src : Logs.LOG)

(* Type of database *)
type t = Indexeddb.Database.t

module Store = Store

let on_update = Store.on_update
let add_graph = Store.add_graph

module Datalog = Datalog

let query = Datalog.query
let query_triple = Datalog.query_triple

(* Return a RDF description for [iri] that is used in the inspect view *)
let get_description db iri =
  let tx = Store.ro_tx db in
  let* s_id_opt = Store.Dictionary.lookup db ~tx (Rdf.Term.of_iri iri) in

  match s_id_opt with
  | Some s_id ->
      let q =
        Datalog.(
          Atom.make "triple"
            Term.
              [
                make_constant @@ Constant.Rdf s_id;
                make_variable "p";
                make_variable "o";
              ])
      in
      Datalog.query_triple db ~tx q
      |> Lwt_seq.fold_left
           (fun graph triple -> Rdf.Graph.add triple graph)
           Rdf.Graph.empty
      >|= Rdf.Graph.description (Rdf.Triple.Subject.of_iri iri)
  | None -> return @@ Rdf.Description.empty (Rdf.Triple.Subject.of_iri iri)

let get_property db subject predicate =
  let tx = Store.ro_tx db in
  let* s_id_opt =
    Store.Dictionary.lookup db ~tx (Rdf.Triple.Subject.to_term subject)
  in
  let* p_id_opt =
    Store.Dictionary.lookup db ~tx (Rdf.Triple.Predicate.to_term predicate)
  in
  match [ s_id_opt; p_id_opt ] with
  | [ Some s_id; Some p_id ] ->
      let q =
        Datalog.(
          Atom.make "triple"
            Term.
              [
                make_constant @@ Constant.Rdf s_id;
                make_constant @@ Constant.Rdf p_id;
                make_variable "o";
              ])
      in
      Datalog.query db ~tx q |> Lwt_seq.return_lwt
      |> Lwt_seq.flat_map (fun set ->
             Lwt_seq.of_seq @@ Datalog.Tuple.Set.to_seq set)
      |> Lwt_seq.filter_map_s (function
           | [ _; _; Datalog.Constant.Rdf o ] -> Store.Dictionary.get db ~tx o
           | _ -> return_none)
      |> Lwt_seq.to_list
  | _ -> return_nil

let get_rdfs_label db iri =
  let* labels =
    get_property db
      (Rdf.Triple.Subject.of_iri iri)
      (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdfs "label")
  in
  labels |> List.find_map (fun term -> Rdf.Term.to_literal term) |> return

let get_with_geo db =
  let q = Datalog.(Atom.make "subject-geo" Term.[ make_variable "s" ]) in

  Datalog.query db q |> Lwt_seq.return_lwt
  |> Lwt_seq.flat_map (fun set ->
         Lwt_seq.of_seq @@ Datalog.Tuple.Set.to_seq set)
  |> Lwt_seq.filter_map_s (function
       | [ Datalog.Constant.Rdf term_id ] -> Store.Dictionary.get db term_id
       | _ -> return_none)
  |> Lwt_seq.filter_map (fun term ->
         Rdf.Term.map term Option.some (fun _ -> None) (fun _ -> None))
  |> Lwt_seq.map_s (get_description db)

let init () =
  let* db = Store.init () in
  let* triple_count = Store.triple_count db in
  Log.debug (fun m -> m "Triples in database: %d" triple_count);

  (* Load some vocabularies if db is empty *)
  let* () =
    if triple_count = 0 then
      let* () =
        Vocabs.vocabs
        |> Lwt_list.iter_p (fun vocab ->
               Log.debug (fun m -> m "Loading vocabulary %s" vocab);
               let* graph = Vocabs.fetch_vocab vocab in
               Store.add_graph db graph)
      in
      Sample_data.sample_data
      |> Lwt_list.iter_p (fun file ->
             Log.debug (fun m -> m "Loading sample data %s" file);
             let* graph = Sample_data.fetch file in
             Store.add_graph db graph)
    else return_unit
  in
  Log.info (fun m -> m "IndexedDB databse initialized.");
  (* Log.debug (fun m -> m "Graph: %a" Rdf.Graph.pp as2); *)
  return db

let test_datalog db =
  (* let () = Datalog.set_debug true in *)
  let q =
    Datalog.(
      Atom.make "geo"
        Term.
          [ make_constant @@ GeoQuery (46.7935, 0.3009, 5); make_variable "s" ])
  in
  let* tuples = Datalog.query db q in

  return
  @@ Log.debug (fun m -> m "test_datalog: %a" Datalog.Tuple.Set.pp tuples)
