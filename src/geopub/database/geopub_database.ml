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

module Store = struct
  (** The IndexedDB backed storage *)

  (** The Store is implemented using following IndexedDB ObjectStores:

- Dictionary: Stores a mapping of RDF Terms to integer keys (auto incremented)
- Triples: Stores triples using keys of terms as stored in Dictionary.

   *)

  let on_update, updated = E.create ()

  let ro_tx db =
    Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadOnly
      [ Jstr.v "dictionary"; Jstr.v "triples" ]

  let rw_tx db =
    Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadWrite
      [ Jstr.v "dictionary"; Jstr.v "triples" ]

  module Dictionary = struct
    let object_store_name = Jstr.v "dictionary"

    open Indexeddb

    let on_version_change db =
      let dictionary =
        Database.create_object_store db
          ~options:(Jv.obj [| ("autoIncrement", Jv.true') |])
          object_store_name
      in

      let _term_index =
        ObjectStore.create_index dictionary ~key_path:[ "term" ]
          ~object_parameters:Jv.(obj [| ("unique", true') |])
        @@ Jstr.v "term"
      in

      ()

    let object_store tx = Transaction.object_store tx object_store_name
    let term_index object_store = ObjectStore.index object_store (Jstr.v "term")

    (* Encoding of Terms *)

    (* Map some constants to hardcoded ids. This allows us to
       formulate Datalog clauses that use the constants defined below. *)
    let constants =
      let activitystreams =
        Rdf.Namespace.make_namespace "https://www.w3.org/ns/activitystreams#"
      in
      [
        (-1, Rdf.Namespace.rdf "type");
        (-2, Rdf.Namespace.rdfs "subPropertyOf");
        (-3, Rdf.Namespace.rdfs "subClassOf");
        (-4, Rdf.Namespace.rdfs "domain");
        (-5, Rdf.Namespace.rdfs "range");
        (-20, activitystreams "Activity");
      ]

    let constant_get id =
      List.assoc_opt id constants |> Option.map Rdf.Term.of_iri

    let constant_lookup term =
      List.find_map
        (fun (id, c) ->
          let c_term = Rdf.Term.of_iri c in
          if Rdf.Term.compare term c_term = 0 then Some id else None)
        constants

    let string_of_term term =
      let encode_iri iri = "<" ^ Rdf.Iri.to_string iri ^ ">" in
      let encode_blank_node bnode = "_:" ^ Rdf.Blank_node.identifier bnode in
      let encode_literal literal =
        match Rdf.Literal.language literal with
        | Some lang -> "\"" ^ Rdf.Literal.canonical literal ^ "\"@" ^ lang
        | None ->
            "\""
            ^ Rdf.Literal.canonical literal
            ^ "\"^^" ^ encode_iri
            @@ Rdf.Literal.datatype literal
      in
      Rdf.Term.map term encode_iri encode_blank_node encode_literal

    let jv_of_term term = Jv.of_string @@ string_of_term term
    let jv_obj_of_term term = Jv.obj [| ("term", jv_of_term term) |]

    let parser =
      let open Angstrom in
      let iri_parser =
        char '<'
        *> (many_till any_char (char '>') >>| List.to_seq >>| String.of_seq)
        >>| Rdf.Iri.of_string
      in
      let whitespace_lst = [ '\x20'; '\x0a'; '\x0d'; '\x09' ] in
      let char_is_not_equal_to lst d = List.for_all (fun x -> x != d) lst in
      let bnode_parser =
        string "_:"
        *> take_while (char_is_not_equal_to ([ ','; ')' ] @ whitespace_lst))
        >>| Rdf.Blank_node.of_string
      in
      let literal_value_parser =
        char '"'
        *> (many_till any_char (char '"') >>| List.to_seq >>| String.of_seq)
      in

      let literal_parser =
        literal_value_parser >>= fun value ->
        choice
          [
            ( char '@'
            *> take_while (char_is_not_equal_to ([ ','; ')' ] @ whitespace_lst))
            >>| fun language -> Rdf.Literal.make_string value ~language );
            ( string "^^" *> iri_parser >>| fun datatype ->
              Rdf.Literal.make value datatype );
          ]
      in

      let make_prefix_parser prefix namespace =
        string prefix *> char ':'
        *> take_while (char_is_not_equal_to ([ ','; ')' ] @ whitespace_lst))
        >>| namespace >>| Rdf.Term.of_iri
      in

      choice
        [
          iri_parser >>| Rdf.Term.of_iri;
          bnode_parser >>| Rdf.Term.of_blank_node;
          literal_parser >>| Rdf.Term.of_literal;
          make_prefix_parser "rdf" Rdf.Namespace.rdf;
          make_prefix_parser "rdfs" Rdf.Namespace.rdfs;
          make_prefix_parser "as"
            (Rdf.Namespace.make_namespace
               "https://www.w3.org/ns/activitystreams#");
          make_prefix_parser "geo"
            (Rdf.Namespace.make_namespace
               "http://www.w3.org/2003/01/geo/wgs84_pos#");
        ]

    let term_of_jv jv =
      Angstrom.parse_string ~consume:Angstrom.Consume.All parser
        (Jv.to_string jv)
      |> Result.to_option

    let term_of_jv_obj jv = Jv.get jv "term" |> term_of_jv

    (* ObjectStore access *)

    let get db ?(tx = ro_tx db) id =
      match constant_get id with
      | Some term -> return_some term
      | None -> (
          let dictionary = object_store tx in
          let* term_jv_opt = ObjectStore.get dictionary (Jv.of_int id) in
          match term_jv_opt with
          | Some term_jv -> term_of_jv (Jv.get term_jv "term") |> return
          | None -> return_none)

    let lookup db ?(tx = ro_tx db) term =
      match constant_lookup term with
      | Some id -> return_some id
      | None ->
          let dictionary = object_store tx in
          let term_index = term_index dictionary in
          Index.get_key term_index (Jv.of_list jv_of_term [ term ])
          >|= Jv.to_option Jv.to_int

    let put db ?(tx = rw_tx db) term =
      let dictionary = object_store tx in
      let* key_opt = lookup db ~tx term in
      match key_opt with
      | None ->
          let* key =
            ObjectStore.put dictionary (jv_obj_of_term term) >|= Jv.to_int
          in
          return key
      | Some key -> return key
  end

  (* The Triple Store *)

  module Triples = struct
    let object_store_name = Jstr.v "triples"

    open Indexeddb

    let object_store tx =
      Indexeddb.Transaction.object_store tx object_store_name

    let on_version_change db =
      (* Create an ObjectStore for triples *)
      let triples =
        Database.create_object_store db
          ~options:(Jv.obj [| ("autoIncrement", Jv.true') |])
          object_store_name
      in

      (* Create the spo index *)
      let _spo_index =
        ObjectStore.create_index triples ~key_path:[ "s"; "p"; "o" ]
          ~object_parameters:Jv.(obj [| ("unique", true') |])
        @@ Jstr.v "spo"
      in

      (* Create the s index *)
      let _s_index =
        ObjectStore.create_index triples ~key_path:[ "s" ]
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "s"
      in

      (* Create the p index *)
      let _p_index =
        ObjectStore.create_index triples ~key_path:[ "p" ]
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "p"
      in

      (* Create the o index *)
      let _p_index =
        ObjectStore.create_index triples ~key_path:[ "o" ]
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "o"
      in

      (* Create the sp index *)
      let _sp_index =
        ObjectStore.create_index triples ~key_path:[ "s"; "p" ]
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "sp"
      in

      (* Create the so index *)
      let _so_index =
        ObjectStore.create_index triples ~key_path:[ "s"; "o" ]
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "so"
      in

      (* Create the po index *)
      let _po_index =
        ObjectStore.create_index triples ~key_path:[ "p"; "o" ]
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "po"
      in

      ()

    (* Encoding *)

    let jv_of_triple s p o =
      Jv.obj [| ("s", Jv.of_int s); ("p", Jv.of_int p); ("o", Jv.of_int o) |]

    let triple_of_jv jv =
      let s = Jv.(to_int @@ get jv "s") in
      let p = Jv.(to_int @@ get jv "p") in
      let o = Jv.(to_int @@ get jv "o") in
      [ s; p; o ]

    (* ObjectStore access *)

    let deref db ?(tx = ro_tx db) triple_ids =
      let* terms = Lwt_list.map_s (Dictionary.get db ~tx) triple_ids in
      match terms with
      | [ Some s; Some p; Some o ] ->
          return_some
          @@ Rdf.Triple.(
               make
                 (Rdf.Term.map s Subject.of_iri Subject.of_blank_node (fun _ ->
                      failwith "unexpcted literal"))
                 (Rdf.Term.map p Predicate.of_iri
                    (fun _ -> failwith "unexpected blank_node")
                    (fun _ -> failwith "unexpected literal"))
                 (Object.of_term o))
      | _ -> return_none

    let put db (triple : Rdf.Triple.t) =
      let tx = rw_tx db in

      (* let triples = Triples.object_store tx in *)
      let* s_id =
        triple.subject |> Rdf.Triple.Subject.to_term |> Dictionary.put db ~tx
      in
      let* p_id =
        triple.predicate |> Rdf.Triple.Predicate.to_term
        |> Dictionary.put db ~tx
      in
      let* o_id =
        triple.object' |> Rdf.Triple.Object.to_term |> Dictionary.put db ~tx
      in
      let triples = object_store tx in

      let spo_index = ObjectStore.index triples (Jstr.v "spo") in

      let* key_opt =
        Index.get_key spo_index (Jv.of_list Jv.of_int [ s_id; p_id; o_id ])
        >|= Jv.to_option Jv.to_int
      in

      match key_opt with
      | None ->
          ObjectStore.put triples (jv_of_triple s_id p_id o_id) >|= Jv.to_int
      | Some key -> return key
  end

  let init () =
    let* db =
      Indexeddb.Database.open' ~version:geopub_database_version
        ~on_version_change:(fun db ->
          Log.debug (fun m -> m "Performing database version change.");

          (* Delete all existing data *)
          List.iter
            (fun object_store_name ->
              Indexeddb.Database.delete_object_store db object_store_name)
            (Indexeddb.Database.object_store_names db);

          (* Initialize Object stores and indices *)
          Dictionary.on_version_change db;
          Triples.on_version_change db)
        (Jstr.v geopub_database_name)
    in

    (* Force update of triple store *)
    updated db;

    return db

  let triple_count db =
    let tx = ro_tx db in
    Indexeddb.ObjectStore.count (Triples.object_store tx) Jv.undefined

  let add_graph db (graph : Rdf.Graph.t) =
    let* () =
      Rdf.Graph.to_triples graph |> List.of_seq
      |> Lwt_list.iter_p (fun triple -> Triples.put db triple >|= ignore)
    in
    return @@ updated db

  let edb tx predicate pattern =
    let parse = Lwt_stream.map Triples.triple_of_jv in

    let jv_of_index idx = Jv.of_list Jv.of_int idx in

    let open Indexeddb in
    (* Open the triples object store *)
    let triples = Transaction.object_store tx Triples.object_store_name in

    (* Get triples with index matching the query pattern *)
    match (predicate, pattern) with
    | "triples", [ None; None; None ] ->
        Log.debug (fun m -> m "EDB: getting all triples");
        ObjectStore.open_cursor triples Jv.undefined
        |> Cursor.to_stream |> parse
    | "triples", [ Some s; None; None ] ->
        Log.debug (fun m -> m "EDB: using s index");
        let s_index = ObjectStore.index triples (Jstr.v "s") in
        Index.open_cursor s_index (jv_of_index [ s ])
        |> Cursor.to_stream |> parse
    | "triples", [ None; Some p; None ] ->
        Log.debug (fun m -> m "EDB: using p index");
        let p_index = ObjectStore.index triples (Jstr.v "p") in
        Index.open_cursor p_index (jv_of_index [ p ])
        |> Cursor.to_stream |> parse
    | "triples", [ None; None; Some o ] ->
        Log.debug (fun m -> m "EDB: using o index");
        let o_index = ObjectStore.index triples (Jstr.v "o") in
        Index.open_cursor o_index (jv_of_index [ o ])
        |> Cursor.to_stream |> parse
    | "triples", [ Some s; Some p; None ] ->
        Log.debug (fun m -> m "EDB: using sp index");
        let sp_index = ObjectStore.index triples (Jstr.v "sp") in
        Index.open_cursor sp_index (jv_of_index [ s; p ])
        |> Cursor.to_stream |> parse
    | "triples", [ Some s; None; Some o ] ->
        Log.debug (fun m -> m "EDB: using so index");
        let so_index = ObjectStore.index triples (Jstr.v "so") in
        Index.open_cursor so_index (jv_of_index [ s; o ])
        |> Cursor.to_stream |> parse
    | "triples", [ None; Some p; Some o ] ->
        Log.debug (fun m -> m "EDB: using po index");
        let po_index = ObjectStore.index triples (Jstr.v "po") in
        Index.open_cursor po_index (jv_of_index [ p; o ])
        |> Cursor.to_stream |> parse
    | "triples", [ Some s; Some p; Some o ] ->
        Log.debug (fun m -> m "EDB: using spo index");
        let spo_index = ObjectStore.index triples (Jstr.v "spo") in
        Index.open_cursor spo_index (jv_of_index [ s; p; o ])
        |> Cursor.to_stream |> parse
    | _, _ -> Lwt_stream.of_list []
end

module Datalog = struct
  include Datalogl.Make (struct
    type t = int

    let compare = Int.compare

    let parser =
      let constant iri =
        Angstrom.(
          match Store.Dictionary.constant_lookup @@ Rdf.Term.of_iri iri with
          | Some id -> return id
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

    let pp = Fmt.int
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
      "rhodf(?a, sp, ?c) :- rhodf(?a, sp, ?b), rhodf(?b, sp, ?c).";
      (* Subproperty (b) *)
      "rhodf(?x, ?b, ?y) :- rhodf(?a, sp, ?b), rhodf(?x, ?a, ?y).";
      (* Subclass (a) *)
      "rhodf(?a, sc, ?c) :- rhodf(?a, sc, ?b), rhodf(?b, sc, ?c).";
      (* Subclass (b) *)
      "rhodf(?x, type, ?b) :- rhodf(?a, sc, ?b), rhodf(?x, type, ?a).";
      (* Typing (a) *)
      "rhodf(?x, type, ?b) :- rhodf(?a, dom, ?b), rhodf(?x, ?a, ?y).";
      (* Typing (b) *)
      "rhodf(?y, type, ?b) :- rhodf(?a, range, ?b), rhodf(?x, ?a, ?y).";
      (* Implicit typing (a) *)
      "rhodf(?x, type, ?b) :- rhodf(?a, dom, ?b), rhodf(?c, sc, ?a), rhodf(?x, \
       ?c, ?y).";
      (* Implicit typing (b) *)
      "rhodf(?y, type, ?b) :- rhodf(?a, range, ?b), rhodf(?c, sc, ?a), \
       rhodf(?x, ?c, ?y)."
      (* Omitting the reflexiv subClassOf, subPropertyOf rules out of lazyness.*);
    ]
    |> String.concat "\n"

  let geopub_datalog_program =
    {datalog|
    rdf(?s,?p,?o) :- triples(?s,?p,?o).
      |datalog}
    (* activity(?s) :- triples(?s,rdf:type,as:Create).
     * activity(?s) :- triples(?s,rdf:type,as:Listen).
     * activity(?s) :- triples(?s,rdf:type,as:Like).
     * withgeo(?s) :- triples(?s, geo:lat, ?lat), triples(?s, geo:long, ?lng).
     * |datalog} *)
    ^ rhodf
    |> Angstrom.parse_string ~consume:Angstrom.Consume.All Program.parser
    |> Result.get_ok

  let geopub_state = ref (init geopub_datalog_program)
end

let add_graph = Store.add_graph

let query db ?(tx = Store.ro_tx db) q =
  let* state, tuples =
    Datalog.(query_with_state ~database:(Store.edb tx) ~state:!geopub_state q)
  in
  Datalog.geopub_state := state;
  return tuples

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
      Store.edb tx "triples" [ Some s_id; None; None ]
      |> Lwt_stream.filter_map_s (Store.Triples.deref db ~tx)
      |> fun stream ->
      Lwt_stream.fold Rdf.Graph.add stream Rdf.Graph.empty
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
      Store.edb tx "triples" [ Some s_id; Some p_id; None ]
      |> Lwt_stream.filter_map_s (function
           | [ _; _; o ] -> Store.Dictionary.get db ~tx o
           | _ -> return_none)
      |> Lwt_stream.to_list
  | _ -> return_nil

let get_rdfs_label db iri =
  let* labels =
    get_property db
      (Rdf.Triple.Subject.of_iri iri)
      (Rdf.Triple.Predicate.of_iri @@ Rdf.Namespace.rdfs "label")
  in
  labels |> List.find_map (fun term -> Rdf.Term.to_literal term) |> return

let get_with_geo db =
  ignore db;
  return_nil
(* query_string db {query|withgeo(?s)|query}
 * >|= Datalog.Tuple.Set.to_seq
 * >|= Seq.filter_map (function
 *       | [ term ] ->
 *           Rdf.Term.map term Option.some (fun _ -> None) (fun _ -> None)
 *       | _ -> None)
 * >|= List.of_seq
 * >>= Lwt_list.map_s (fun iri -> get_description db iri) *)

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

let delete db =
  Log.info (fun m -> m "Deleting IndexedDB databse.");
  Datalog.geopub_state := Datalog.init Datalog.geopub_datalog_program;
  Indexeddb.(
    return @@ Database.close db >>= fun () ->
    Database.delete (Jstr.v geopub_database_name))

let test_datalog db =
  let tx = Store.ro_tx db in
  let* tuples = query_string db ~tx {query|rhodf(?s,sc,?o)|query} in

  let* triples =
    tuples |> Datalog.Tuple.Set.to_rev_seq |> List.of_seq
    |> Lwt_list.filter_map_p (Store.Triples.deref db ~tx)
  in

  return
  @@ Log.debug (fun m -> m "test_datalog: %a" Fmt.(list Rdf.Triple.pp) triples)
