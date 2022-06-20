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
  type t = Rdf of int | FtsQuery of string | GeoQuery of float * float * int

  let compare a b =
    let compare3 c0 a0 b0 c1 a1 b1 c2 a2 b2 =
      if c0 a0 b0 == 0 then if c1 a1 b1 == 0 then c2 a2 b2 else c1 a1 b1
      else c0 a0 b0
    in
    match (a, b) with
    | Rdf a, Rdf b -> Int.compare a b
    | Rdf _, FtsQuery _ -> -1
    | Rdf _, GeoQuery _ -> -1
    | FtsQuery _, Rdf _ -> 1
    | FtsQuery _, GeoQuery _ -> -1
    | FtsQuery a, FtsQuery b -> String.compare a b
    | ( GeoQuery (a_lat, a_long, a_precision),
        GeoQuery (b_lat, b_long, b_precision) ) ->
        compare3 Float.compare a_lat b_lat Float.compare a_long b_long
          Int.compare a_precision b_precision
    | GeoQuery _, _ -> 1

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
    | GeoQuery (lat, long, precision) ->
        Fmt.pf ppf "GeoHash(%f, %f, %d)" lat long precision
end

include Datalogl.Make (Constant)

let edb tx predicate pattern =
  let open Indexeddb in
  (* Open the triples object store *)
  let triples = Transaction.object_store tx Store.Triples.object_store_name in

  let query index q =
    let keyrange q =
      let jv_of_index q = Jv.of_list Jv.of_int q in
      let jv_upper_bound q =
        Jv.(
          of_list
            (function
              | Some i -> Jv.of_int i
              | None ->
                  (* Use a empty string for the upper bound as strings are
                     always ordered higher than integers *)
                  Jv.of_string "")
            (List.map Option.some q @ [ None ]))
      in
      KeyRange.bound (jv_of_index q) (jv_upper_bound q)
    in
    let triples_of_cursor (cursor_promise : Cursor.t option Lwt.t) =
      cursor_promise |> Cursor.opt_lwt_to_seq |> Lwt_seq.map Cursor.value
      |> Lwt_seq.map Store.Triples.triple_of_jv
      |> Lwt_seq.map (fun int_triples ->
             List.map (fun i -> Constant.Rdf i) int_triples)
    in
    let index = ObjectStore.index triples (Jstr.v index) in
    Index.open_cursor index (keyrange q) |> triples_of_cursor
  in

  (* Get triples with index matching the query pattern *)
  match (predicate, pattern) with
  | "triple", [ None; None; None ] ->
      Log.warn (fun m -> m "EDB: getting all triples");
      query "spo" []
  | "triple", [ Some (Constant.Rdf s); None; None ] -> query "spo" [ s ]
  | "triple", [ None; Some (Constant.Rdf p); None ] -> query "pos" [ p ]
  | "triple", [ None; None; Some (Constant.Rdf o) ] -> query "osp" [ o ]
  | "triple", [ Some (Constant.Rdf s); Some (Constant.Rdf p); None ] ->
      query "spo" [ s; p ]
  | "triple", [ Some (Constant.Rdf s); None; Some (Constant.Rdf o) ] ->
      query "osp" [ o; s ]
  | "triple", [ None; Some (Constant.Rdf p); Some (Constant.Rdf o) ] ->
      query "pos" [ p; o ]
  | ( "triple",
      [ Some (Constant.Rdf s); Some (Constant.Rdf p); Some (Constant.Rdf o) ] )
    ->
      query "spo" [ s; p; o ]
  | "fts", [ Some (Constant.FtsQuery s); None ] ->
      Store.Fts.search tx s
      |> Lwt_seq.map (fun term_id ->
             [ Constant.FtsQuery s; Constant.Rdf term_id ])
  | ( "geo",
      [
        Some (Constant.GeoQuery (lat, long, precision) as geo_query_constant);
        None;
      ] ) ->
      Store.Geo.search tx (lat, long, precision)
      |> Lwt_seq.map (fun term_id ->
             [ geo_query_constant; Constant.Rdf term_id ])
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
