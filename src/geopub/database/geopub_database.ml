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

type t = Indexeddb.Database.t

(* Constants *)
let geopub_database_version = 1
let geopub_database_name = "GeoPub"
let triples_object_store_name = Jstr.v "triples"

(* The Triple Store *)

module Triples = struct
  let on_update, updated = E.create ()

  let count db =
    let tx =
      Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadWrite
        [ triples_object_store_name ]
    in
    let triples =
      Indexeddb.Transaction.object_store tx triples_object_store_name
    in
    Indexeddb.ObjectStore.count triples Jv.undefined

  let add tx (triple : Rdf.Triple.t) =
    let triples =
      Indexeddb.Transaction.object_store tx triples_object_store_name
    in
    let spo = Indexeddb.ObjectStore.index triples (Jstr.v "spo") in
    let* count =
      Indexeddb.Index.count spo
        (Encoding.jv_of_terms
           [
             Rdf.Triple.Subject.to_term triple.subject;
             Rdf.Triple.Predicate.to_term triple.predicate;
             Rdf.Triple.Object.to_term triple.object';
           ])
    in
    if count = 0 then
      (* TODO usage of jv_of_triple is inefficient as we have already encoded triples into Buffers above. This should be reused. *)
      let* _ =
        Indexeddb.ObjectStore.add triples (Encoding.jv_of_triple triple)
      in
      return_unit
    else return_unit

  let add_graph db graph =
    let tx =
      Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadWrite
        [ triples_object_store_name ]
    in
    let* () =
      Rdf.Graph.to_triples graph |> List.of_seq |> Lwt_list.iter_p (add tx)
    in

    updated db;

    return @@ Indexeddb.Transaction.commit tx
end

module Datalog = struct
  include Datalogl.Make (struct
    type t = Rdf.Term.t

    let compare = Rdf.Term.compare
    let parser = Encoding.parser
    let pp = Rdf.Term.pp
  end)

  (* The ρdf fragment of RDF
   * See: Muñoz, S., Pérez, J., & Gutierrez, C. (2009). Simple and
   Efficient Minimal RDFS. Web Semantics: Science, Services and Agents
   on the World Wide Web, 7(3),
   220–234. doi:10.1016/j.websem.2009.07.003 *)
  let rhodf =
    [
      (* rhodf is an extension of rdf. This corresponds to the simple rules in the paper. *)
      "rhodf(?s,?p,?o) :- triples(?s,?p,?o).";
      (* Subproperty (a) *)
      "rhodf(?a, rdfs:subPropertyOf, ?c) :- rhodf(?a, rdfs:subPropertyOf, ?b), \
       rhodf(?b, rdfs:subPropertyOf, ?c).";
      (* Subproperty (b) *)
      "rhodf(?x, ?b, ?y) :- rhodf(?a, rdfs:subPropertyOf, ?b), rhodf(?x, ?a, \
       ?y).";
      (* Subclass (a) *)
      "rhodf(?a, rdfs:subClassOf, ?c) :- rhodf(?a, rdfs:subClassOf, ?b), \
       rhodf(?b, rdfs:subClassOf, ?c).";
      (* Subclass (b) *)
      "rhodf(?x, rdf:type, ?b) :- rhodf(?a, rdfs:subClassOf, ?b), rhodf(?x, \
       rdf:type, ?a).";
      (* Typing (a) *)
      "rhodf(?x, rdf:type, ?b) :- rhodf(?a, rdfs:domain, ?b), rhodf(?x, ?a, \
       ?y).";
      (* Typing (b) *)
      "rhodf(?y, rdf:type, ?b) :- rhodf(?a, rdfs:range, ?b), rhodf(?x, ?a, ?y).";
      (* Implicit typing (a) *)
      "rhodf(?x, rdf:type, ?b) :- rhodf(?a, rdfs:domain, ?b), rhodf(?c, \
       rdfs:subClassOf, ?a), rhodf(?x, ?c, ?y).";
      (* Implicit typing (b) *)
      "rhodf(?y, rdf:type, ?b) :- rhodf(?a, rdfs:range, ?b), rhodf(?c, \
       rdfs:subClassOf, ?a), rhodf(?x, ?c, ?y)."
      (* Omitting the reflexiv subClassOf, subPropertyOf rules out of lazyness.*);
    ]
    |> String.concat "\n"

  let geopub_datalog_program =
    {datalog|
   rdf(?s,?p,?o) :- triples(?s,?p,?o).
   activity(?s) :- triples(?s,rdf:type,as:Create).
   activity(?s) :- triples(?s,rdf:type,as:Listen).
   activity(?s) :- triples(?s,rdf:type,as:Like).
   withgeo(?s) :- triples(?s, geo:lat, ?lat), triples(?s, geo:long, ?lng).
   |datalog}
    ^ rhodf
    |> Angstrom.parse_string ~consume:Angstrom.Consume.All Program.parser
    |> Result.get_ok

  let geopub_state = ref (init geopub_datalog_program)

  let edb tx predicate pattern =
    let parse =
      Lwt_stream.filter_map (fun jv ->
          match Encoding.tuple_of_jv jv with
          | Ok tuple -> Some tuple
          | Error msg ->
              Log.warn (fun m ->
                  m "Encountered tuple that could not be parsed (%s)" msg);
              Brr.Console.warn [ jv ];
              None)
    in

    let open Indexeddb in
    (* Open the triples object store *)
    let triples = Transaction.object_store tx triples_object_store_name in
    (* Get triples with index matching the query pattern *)
    match (predicate, pattern) with
    | "triples", [ None; None; None ] ->
        Log.debug (fun m -> m "EDB: getting all triples");
        ObjectStore.open_cursor triples Jv.undefined
        |> Cursor.to_stream |> parse
    | "triples", [ Some s; None; None ] ->
        Log.debug (fun m -> m "EDB: using s index");
        let s_index = ObjectStore.index triples (Jstr.v "s") in
        Index.open_cursor s_index Encoding.(jv_of_terms [ s ])
        |> Cursor.to_stream |> parse
    | "triples", [ None; Some p; None ] ->
        Log.debug (fun m -> m "EDB: using p index");
        let p_index = ObjectStore.index triples (Jstr.v "p") in
        Index.open_cursor p_index Encoding.(jv_of_terms [ p ])
        |> Cursor.to_stream |> parse
    | "triples", [ None; None; Some o ] ->
        Log.debug (fun m -> m "EDB: using o index");
        let o_index = ObjectStore.index triples (Jstr.v "o") in
        Index.open_cursor o_index Encoding.(jv_of_terms [ o ])
        |> Cursor.to_stream |> parse
    | "triples", [ Some s; Some p; None ] ->
        Log.debug (fun m -> m "EDB: using sp index");
        let sp_index = ObjectStore.index triples (Jstr.v "sp") in
        Index.open_cursor sp_index Encoding.(jv_of_terms [ s; p ])
        |> Cursor.to_stream |> parse
    | "triples", [ Some s; None; Some o ] ->
        Log.debug (fun m -> m "EDB: using so index");
        let so_index = ObjectStore.index triples (Jstr.v "so") in
        Index.open_cursor so_index Encoding.(jv_of_terms [ s; o ])
        |> Cursor.to_stream |> parse
    | "triples", [ None; Some p; Some o ] ->
        Log.debug (fun m -> m "EDB: using po index");
        let po_index = ObjectStore.index triples (Jstr.v "po") in
        Index.open_cursor po_index Encoding.(jv_of_terms [ p; o ])
        |> Cursor.to_stream |> parse
    | "triples", [ Some s; Some p; Some o ] ->
        Log.debug (fun m -> m "EDB: using spo index");
        let spo_index = ObjectStore.index triples (Jstr.v "spo") in
        Index.open_cursor spo_index Encoding.(jv_of_terms [ s; p; o ])
        |> Cursor.to_stream |> parse
    | _, _ -> Lwt_stream.of_list []
end

let init () =
  let* db =
    Indexeddb.Database.open' ~version:geopub_database_version
      ~on_version_change:(fun db ->
        let open Indexeddb.Database.VersionChange in
        Log.debug (fun m -> m "Performing database version change.");

        (* Create an ObjectStore for triples *)
        let triples =
          create_object_store db
            ~options:(Jv.obj [| ("autoIncrement", Jv.true') |])
            triples_object_store_name
        in

        (* Create the spo index *)
        let _spo_index =
          create_index triples ~key_path:[ "s"; "p"; "o" ]
            ~object_parameters:Jv.(obj [| ("unique", true') |])
          @@ Jstr.v "spo"
        in

        (* Create the s index *)
        let _s_index =
          create_index triples ~key_path:[ "s" ]
            ~object_parameters:Jv.(obj [| ("unique", false') |])
          @@ Jstr.v "s"
        in

        (* Create the p index *)
        let _p_index =
          create_index triples ~key_path:[ "p" ]
            ~object_parameters:Jv.(obj [| ("unique", false') |])
          @@ Jstr.v "p"
        in

        (* Create the o index *)
        let _p_index =
          create_index triples ~key_path:[ "o" ]
            ~object_parameters:Jv.(obj [| ("unique", false') |])
          @@ Jstr.v "o"
        in

        (* Create the sp index *)
        let _sp_index =
          create_index triples ~key_path:[ "s"; "p" ]
            ~object_parameters:Jv.(obj [| ("unique", false') |])
          @@ Jstr.v "sp"
        in

        (* Create the so index *)
        let _so_index =
          create_index triples ~key_path:[ "s"; "o" ]
            ~object_parameters:Jv.(obj [| ("unique", false') |])
          @@ Jstr.v "so"
        in

        (* Create the po index *)
        let _po_index =
          create_index triples ~key_path:[ "p"; "o" ]
            ~object_parameters:Jv.(obj [| ("unique", false') |])
          @@ Jstr.v "po"
        in

        ())
      (Jstr.v geopub_database_name)
  in

  (* Force update of triple store *)
  Triples.updated db;

  let* triple_count = Triples.count db in
  Log.debug (fun m -> m "Triples in database: %d" triple_count);

  (* Load some vocabularies if db is empty*)
  let* () =
    if triple_count = 0 then
      Vocabs.vocabs
      |> Lwt_list.iter_p (fun vocab ->
             Log.debug (fun m -> m "Loading vocabulary %s" vocab);
             let* graph = Vocabs.fetch_vocab vocab in
             Triples.add_graph db graph)
    else return_unit
  in

  Log.info (fun m -> m "IndexedDB databse initialized.");
  (* Log.debug (fun m -> m "Graph: %a" Rdf.Graph.pp as2); *)
  return db

let delete db =
  Log.info (fun m -> m "Deleting IndexedDB databse.");
  Datalog.geopub_state := Datalog.init Datalog.geopub_datalog_program;
  Indexeddb.(
    return @@ Database.close db >>= fun () ->
    Database.delete (Jstr.v geopub_database_name))

let query db q =
  let tx =
    Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadOnly
      [ triples_object_store_name ]
  in
  let* state, tuples =
    Datalog.(query_with_state ~database:(edb tx) ~state:!geopub_state q)
  in
  Datalog.geopub_state := state;
  return tuples

let query_string db q =
  let* q =
    q |> Angstrom.parse_string ~consume:Angstrom.Consume.All Datalog.Atom.parser
    |> function
    | Ok query -> return query
    | Error msg -> Lwt.fail_with msg
  in
  query db q

(* Return a RDF description for [iri] that is used in the inspect view *)
let get_description db iri =
  let tx =
    Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadOnly
      [ triples_object_store_name ]
  in
  Datalog.edb tx "triples" [ Some (Rdf.Term.of_iri iri); None; None ]
  |> Lwt_stream.filter_map (function
       | [ s; p; o ] ->
           Rdf.Triple.(
             Option.some
             @@ make
                  (Rdf.Term.map s Subject.of_iri Subject.of_blank_node (fun _ ->
                       failwith "unexpected literal in subject position"))
                  (Rdf.Term.map p Predicate.of_iri
                     (fun _ ->
                       failwith "unexpected blank node in predicate position")
                     (fun _ ->
                       failwith "unexpected literal in predicate position"))
                  (Object.of_term o))
       | _ -> None)
  |> fun stream ->
  Lwt_stream.fold Rdf.Graph.add stream Rdf.Graph.empty
  >|= Rdf.Graph.description (Rdf.Triple.Subject.of_iri iri)

let get_property db subject predicate =
  let tx =
    Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadOnly
      [ triples_object_store_name ]
  in
  Datalog.edb tx "triples"
    [
      Some (Rdf.Triple.Subject.to_term subject);
      Some (Rdf.Triple.Predicate.to_term predicate);
      None;
    ]
  |> Lwt_stream.filter_map (function [ _; _; o ] -> Some o | _ -> None)
  |> Lwt_stream.to_list

let get_rdfs_label db iri =
  let* labels =
    get_property db
      (Rdf.Triple.Subject.of_iri iri)
      (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdfs "label")
  in
  labels |> List.find_map (fun term -> Rdf.Term.to_literal term) |> return

let get_activities db =
  query_string db {query|activity(?s)|query}
  >|= Datalog.Tuple.Set.to_seq
  >|= Seq.filter_map (function
        | [ term ] ->
            Rdf.Term.map term Option.some (fun _ -> None) (fun _ -> None)
        | _ -> None)
  >|= List.of_seq
  >>= Lwt_list.map_s (fun iri -> get_description db iri)

let get_with_geo db =
  query_string db {query|withgeo(?s)|query}
  >|= Datalog.Tuple.Set.to_seq
  >|= Seq.filter_map (function
        | [ term ] ->
            Rdf.Term.map term Option.some (fun _ -> None) (fun _ -> None)
        | _ -> None)
  >|= List.of_seq
  >>= Lwt_list.map_s (fun iri -> get_description db iri)

let test_datalog db =
  let* tuples = query_string db {query|withgeo(?s)|query} in

  Log.debug (fun m -> m "test_datalog: %a" Datalog.Tuple.Set.pp tuples);
  return_unit
