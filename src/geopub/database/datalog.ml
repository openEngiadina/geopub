(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Database.Datalog"

module Log = (val Logs.src_log src : Logs.LOG)

module Constant = struct
  type t = Rdf of int | FtsQuery of string

  let compare a b =
    match (a, b) with
    | Rdf a, Rdf b -> Int.compare a b
    | Rdf _, FtsQuery _ -> -1
    | FtsQuery _, Rdf _ -> 1
    | FtsQuery a, FtsQuery b -> String.compare a b

  let parser =
    let constant iri =
      Angstrom.(
        match Store.Dictionary.constant_lookup @@ Rdf.Term.of_iri iri with
        | Some id -> return (Rdf id)
        | None -> fail "not a valid term")
    in
    Angstrom.(
      choice ~failure_msg:"not a valid term"
        [
          string "type" *> (constant @@ Rdf.Namespace.rdf "type");
          string "sp" *> (constant @@ Rdf.Namespace.rdfs "subPropertyOf");
          string "sc" *> (constant @@ Rdf.Namespace.rdfs "subClassOf");
          string "dom" *> (constant @@ Rdf.Namespace.rdfs "domain");
          string "range" *> (constant @@ Rdf.Namespace.rdfs "range");
        ])

  let pp ppf t =
    match t with
    | Rdf t -> Fmt.pf ppf "%a" Fmt.int t
    | FtsQuery t -> Fmt.pf ppf "\"%s\"" t
end

include Datalogl.Make (Constant)

let edb tx predicate pattern =
  let open Indexeddb in
  let triples_of_cursor (cursor_promise : Cursor.t option Lwt.t) =
    cursor_promise |> Cursor.opt_lwt_to_seq |> Lwt_seq.map Cursor.value
    |> Lwt_seq.map Store.Triples.triple_of_jv
    |> Lwt_seq.map (fun int_triples ->
           List.map (fun i -> Constant.Rdf i) int_triples)
  in

  let jv_of_index idx = Jv.of_list Jv.of_int idx in

  (* Open the triples object store *)
  let triples = Transaction.object_store tx Store.Triples.object_store_name in

  (* Get triples with index matching the query pattern *)
  match (predicate, pattern) with
  | "triple", [ None; None; None ] ->
      Log.warn (fun m -> m "EDB: getting all triples");
      ObjectStore.open_cursor triples Jv.undefined |> triples_of_cursor
  | "triple", [ Some (Constant.Rdf s); None; None ] ->
      (* Log.debug (fun m -> m "EDB: using s index"); *)
      let s_index = ObjectStore.index triples (Jstr.v "s") in
      Index.open_cursor s_index (KeyRange.only @@ jv_of_index [ s ])
      |> triples_of_cursor
  | "triple", [ None; Some (Constant.Rdf p); None ] ->
      (* Log.debug (fun m -> m "EDB: using p index"); *)
      let p_index = ObjectStore.index triples (Jstr.v "p") in
      Index.open_cursor p_index (KeyRange.only @@ jv_of_index [ p ])
      |> triples_of_cursor
  | "triple", [ None; None; Some (Constant.Rdf o) ] ->
      (* Log.debug (fun m -> m "EDB: using o index"); *)
      let o_index = ObjectStore.index triples (Jstr.v "o") in
      Index.open_cursor o_index (KeyRange.only @@ jv_of_index [ o ])
      |> triples_of_cursor
  | "triple", [ Some (Constant.Rdf s); Some (Constant.Rdf p); None ] ->
      (* Log.debug (fun m -> m "EDB: using sp index"); *)
      let sp_index = ObjectStore.index triples (Jstr.v "sp") in
      Index.open_cursor sp_index (KeyRange.only @@ jv_of_index [ s; p ])
      |> triples_of_cursor
  | "triple", [ Some (Constant.Rdf s); None; Some (Constant.Rdf o) ] ->
      (* Log.debug (fun m -> m "EDB: using so index"); *)
      let so_index = ObjectStore.index triples (Jstr.v "so") in
      Index.open_cursor so_index (KeyRange.only @@ jv_of_index [ s; o ])
      |> triples_of_cursor
  | "triple", [ None; Some (Constant.Rdf p); Some (Constant.Rdf o) ] ->
      (* Log.debug (fun m -> m "EDB: using po index"); *)
      let po_index = ObjectStore.index triples (Jstr.v "po") in
      Index.open_cursor po_index (KeyRange.only @@ jv_of_index [ p; o ])
      |> triples_of_cursor
  | ( "triple",
      [ Some (Constant.Rdf s); Some (Constant.Rdf p); Some (Constant.Rdf o) ] )
    ->
      (* Log.debug (fun m -> m "EDB: using spo index"); *)
      let spo_index = ObjectStore.index triples (Jstr.v "spo") in
      Index.open_cursor spo_index (KeyRange.only @@ jv_of_index [ s; p; o ])
      |> triples_of_cursor
  | "fts", [ Some (FtsQuery s); None ] ->
      Store.Fts.search tx s
      |> Lwt_seq.map (fun term_id ->
             [ Constant.FtsQuery s; Constant.Rdf term_id ])
  | _, _ -> Lwt_seq.empty

(* The ρdf fragment of RDF
 * See: Muñoz, S., Pérez, J., & Gutierrez, C. (2009). Simple and
   Efficient Minimal RDFS. Web Semantics: Science, Services and Agents
   on the World Wide Web, 7(3),
   220–234. doi:10.1016/j.websem.2009.07.003 *)
let rhodf =
  [
    (* rhodf is an extension of rdf. This corresponds to the simple rules in the paper. *)
    "triple-rhodf(?s,?p,?o) :- triple(?s,?p,?o).";
    (* Subproperty (a) *)
    "triple-rhodf(?a, sp, ?c) :- triple-rhodf(?a, sp, ?b), triple-rhodf(?b, \
     sp, ?c).";
    (* Subproperty (b) *)
    "triple-rhodf(?x, ?b, ?y) :- triple-rhodf(?a, sp, ?b), triple-rhodf(?x, \
     ?a, ?y).";
    (* Subclass (a) *)
    "triple-rhodf(?a, sc, ?c) :- triple-rhodf(?a, sc, ?b), triple-rhodf(?b, \
     sc, ?c).";
    (* Subclass (b) *)
    "triple-rhodf(?x, type, ?b) :- triple-rhodf(?a, sc, ?b), triple-rhodf(?x, \
     type, ?a).";
    (* Typing (a) *)
    "triple-rhodf(?x, type, ?b) :- triple-rhodf(?a, dom, ?b), triple-rhodf(?x, \
     ?a, ?y).";
    (* Typing (b) *)
    "triple-rhodf(?y, type, ?b) :- triple-rhodf(?a, range, ?b), \
     triple-rhodf(?x, ?a, ?y).";
    (* Implicit typing (a) *)
    "triple-rhodf(?x, type, ?b) :- triple-rhodf(?a, dom, ?b), triple-rhodf(?c, \
     sc, ?a), triple-rhodf(?x, ?c, ?y).";
    (* Implicit typing (b) *)
    "triple-rhodf(?y, type, ?b) :- triple-rhodf(?a, range, ?b), \
     triple-rhodf(?c, sc, ?a), triple-rhodf(?x, ?c, ?y)."
    (* Omitting the reflexiv subClassOf, subPropertyOf rules out of lazyness.*);
  ]
  |> String.concat "\n"

let geopub_datalog_program =
  rhodf ^ "triple-fts(?s, ?p, ?o, ?q) :- fts(?q,?o), triple(?s, ?p, ?o)."
  |> Angstrom.parse_string ~consume:Angstrom.Consume.All Program.parser
  |> function
  | Ok program -> program
  | Error msg ->
      Log.err (fun m -> m "Could not parse Datalog program: %s" msg);
      failwith ("Invalid Datalog program: " ^ msg)

let query db ?(tx = Store.ro_tx db) q =
  try query ~database:(edb tx) ~program:geopub_datalog_program q
  with e ->
    Log.err (fun m ->
        m "Error while performing Datalog query (probably wrong arity): %s"
          (Printexc.to_string e));
    return Tuple.Set.empty

let query_triple db ?(tx = Store.ro_tx db) q : Rdf.Triple.t Lwt_seq.t =
  query db ~tx q |> Lwt_seq.return_lwt
  |> Lwt_seq.flat_map (fun set -> Lwt_seq.of_seq @@ Tuple.Set.to_seq set)
  |> Lwt_seq.filter_map_s (function
       | [ Constant.Rdf s_id; Constant.Rdf p_id; Constant.Rdf o_id ] ->
           Store.Triples.deref db ~tx [ s_id; p_id; o_id ]
       | _ -> return_none)
