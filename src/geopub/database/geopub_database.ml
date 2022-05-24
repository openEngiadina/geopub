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

module Datalog = struct
  type term = Term of int | String of string

  include Datalogl.Make (struct
    type t = term

    let compare a b =
      match (a, b) with
      | Term a, Term b -> Int.compare a b
      | Term _, String _ -> -1
      | String _, Term _ -> 1
      | String a, String b -> String.compare a b

    let parser =
      let constant iri =
        Angstrom.(
          match Store.Dictionary.constant_lookup @@ Rdf.Term.of_iri iri with
          | Some id -> return (Term id)
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
      | Term t -> Fmt.pf ppf "%a" Fmt.int t
      | String t -> Fmt.pf ppf "%s" t
  end)

  let edb tx predicate pattern =
    let open Indexeddb in
    let triples_of_cursor (cursor_promise : Cursor.t option Lwt.t) =
      cursor_promise |> Cursor.opt_lwt_to_seq |> Lwt_seq.map Cursor.value
      |> Lwt_seq.map Store.Triples.triple_of_jv
      |> Lwt_seq.map (fun int_triples -> List.map (fun i -> Term i) int_triples)
    in

    let jv_of_index idx = Jv.of_list Jv.of_int idx in

    (* Open the triples object store *)
    let triples = Transaction.object_store tx Store.Triples.object_store_name in

    (* Get triples with index matching the query pattern *)
    match (predicate, pattern) with
    | "triple", [ None; None; None ] ->
        Log.warn (fun m -> m "EDB: getting all triples");
        ObjectStore.open_cursor triples Jv.undefined |> triples_of_cursor
    | "triple", [ Some (Term s); None; None ] ->
        (* Log.debug (fun m -> m "EDB: using s index"); *)
        let s_index = ObjectStore.index triples (Jstr.v "s") in
        Index.open_cursor s_index (jv_of_index [ s ]) |> triples_of_cursor
    | "triple", [ None; Some (Term p); None ] ->
        (* Log.debug (fun m -> m "EDB: using p index"); *)
        let p_index = ObjectStore.index triples (Jstr.v "p") in
        Index.open_cursor p_index (jv_of_index [ p ]) |> triples_of_cursor
    | "triple", [ None; None; Some (Term o) ] ->
        (* Log.debug (fun m -> m "EDB: using o index"); *)
        let o_index = ObjectStore.index triples (Jstr.v "o") in
        Index.open_cursor o_index (jv_of_index [ o ]) |> triples_of_cursor
    | "triple", [ Some (Term s); Some (Term p); None ] ->
        (* Log.debug (fun m -> m "EDB: using sp index"); *)
        let sp_index = ObjectStore.index triples (Jstr.v "sp") in
        Index.open_cursor sp_index (jv_of_index [ s; p ]) |> triples_of_cursor
    | "triple", [ Some (Term s); None; Some (Term o) ] ->
        (* Log.debug (fun m -> m "EDB: using so index"); *)
        let so_index = ObjectStore.index triples (Jstr.v "so") in
        Index.open_cursor so_index (jv_of_index [ s; o ]) |> triples_of_cursor
    | "triple", [ None; Some (Term p); Some (Term o) ] ->
        (* Log.debug (fun m -> m "EDB: using po index"); *)
        let po_index = ObjectStore.index triples (Jstr.v "po") in
        Index.open_cursor po_index (jv_of_index [ p; o ]) |> triples_of_cursor
    | "triple", [ Some (Term s); Some (Term p); Some (Term o) ] ->
        (* Log.debug (fun m -> m "EDB: using spo index"); *)
        let spo_index = ObjectStore.index triples (Jstr.v "spo") in
        Index.open_cursor spo_index (jv_of_index [ s; p; o ])
        |> triples_of_cursor
    | "fts", [ Some (String s); None ] ->
        Store.Fts.search tx s
        |> Lwt_seq.map (fun term_id -> [ String s; Term term_id ])
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
      "triple-rhodf(?x, type, ?b) :- triple-rhodf(?a, sc, ?b), \
       triple-rhodf(?x, type, ?a).";
      (* Typing (a) *)
      "triple-rhodf(?x, type, ?b) :- triple-rhodf(?a, dom, ?b), \
       triple-rhodf(?x, ?a, ?y).";
      (* Typing (b) *)
      "triple-rhodf(?y, type, ?b) :- triple-rhodf(?a, range, ?b), \
       triple-rhodf(?x, ?a, ?y).";
      (* Implicit typing (a) *)
      "triple-rhodf(?x, type, ?b) :- triple-rhodf(?a, dom, ?b), \
       triple-rhodf(?c, sc, ?a), triple-rhodf(?x, ?c, ?y).";
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
end

let add_graph = Store.add_graph

let query db ?(tx = Store.ro_tx db) q =
  Datalog.query ~database:(Datalog.edb tx)
    ~program:Datalog.geopub_datalog_program q

let query_triples db ?(tx = Store.ro_tx db) q : Rdf.Triple.t Lwt_seq.t =
  query db ~tx q |> Lwt_seq.return_lwt
  |> Lwt_seq.flat_map (fun set ->
         Lwt_seq.of_seq @@ Datalog.Tuple.Set.to_seq set)
  |> Lwt_seq.filter_map_s (function
       | [ Datalog.Term s_id; Datalog.Term p_id; Datalog.Term o_id ] ->
           Store.Triples.deref db ~tx [ s_id; p_id; o_id ]
       | _ -> return_none)

(* >|= List.of_seq
 * >>= Lwt_list.filter_map_p (Store.Triples.deref database) *)

let query_string db ?tx q =
  let* q =
    q |> Angstrom.parse_string ~consume:Angstrom.Consume.All Datalog.Atom.parser
    |> function
    | Ok query -> return query
    | Error msg -> Lwt.fail_with msg
  in
  query db ?tx q

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
                make_constant @@ Term s_id; make_variable "p"; make_variable "o";
              ])
      in
      query_triples db ~tx q
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
                make_constant @@ Term s_id;
                make_constant @@ Term p_id;
                make_variable "o";
              ])
      in
      query db ~tx q |> Lwt_seq.return_lwt
      |> Lwt_seq.flat_map (fun set ->
             Lwt_seq.of_seq @@ Datalog.Tuple.Set.to_seq set)
      |> Lwt_seq.filter_map_s (function
           | [ _; _; Datalog.Term o ] -> Store.Dictionary.get db ~tx o
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
  let q =
    Datalog.(
      Atom.make "triple-rhodf"
        Term.
          [
            make_variable "s";
            (* rdf:type *)
            make_constant @@ Term (-1);
            (* geo:SpatialThing *)
            make_constant @@ Term (-32);
          ])
  in

  query db q |> Lwt_seq.return_lwt
  |> Lwt_seq.flat_map (fun set ->
         Lwt_seq.of_seq @@ Datalog.Tuple.Set.to_seq set)
  |> Lwt_seq.filter_map_s (function
       | [ Datalog.Term term_id; _; _ ] -> Store.Dictionary.get db term_id
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
      Vocabs.vocabs
      |> Lwt_list.iter_p (fun vocab ->
             Log.debug (fun m -> m "Loading vocabulary %s" vocab);
             let* graph = Vocabs.fetch_vocab vocab in
             Store.add_graph db graph)
    else return_unit
  in
  Log.info (fun m -> m "IndexedDB databse initialized.");
  (* Log.debug (fun m -> m "Graph: %a" Rdf.Graph.pp as2); *)
  return db

let () = Datalog.set_debug true

let test_datalog db =
  let q =
    Datalog.(
      Atom.make "triple-fts"
        Term.
          [
            make_variable "s";
            make_variable "p";
            make_variable "o";
            make_constant @@ String "something";
          ])
  in
  let* tuples = query db q in

  (* let* () =
   *   Store.Fts.search (Store.ro_tx db) "you"
   *   |> Lwt_seq.fold_left (fun () id -> Brr.Console.log [ Jv.of_int id ]) ()
   * in *)
  return
  @@ Log.debug (fun m -> m "test_datalog: %a" Datalog.Tuple.Set.pp tuples)
