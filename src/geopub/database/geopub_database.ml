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

(* Namespaces *)
let geo =
  Rdf.Namespace.make_namespace "http://www.w3.org/2003/01/geo/wgs84_pos#"

(* Type of database *)
type t = Indexeddb.Database.t

(* Event that signals new data in database. *)
let on_update, updated = E.create ()

(* Constants *)
let geopub_database_version = 3
let geopub_database_name = "GeoPub"

module Store = struct
  (** The IndexedDB backed storage *)

  (** The Store is implemented using following IndexedDB ObjectStores:

- Fts: A full-text search index that maps stemmed terms to literals.
- Dictionary: Stores a mapping of RDF Terms to integer keys (auto incremented).
- Triples: Stores triples using keys of terms as stored in Dictionary.

   *)

  let ro_tx db =
    Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadOnly
      [ Jstr.v "dictionary"; Jstr.v "triples"; Jstr.v "fts" ]

  let rw_tx db =
    Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadWrite
      [ Jstr.v "dictionary"; Jstr.v "triples"; Jstr.v "fts" ]

  (* The Full-Text-Search index

     This maintains an index from stemmed terms to literals.

     The primary key of entries is the id as stored in the dictionary for the term.

     Inspiration comes from this gist: https://gist.github.com/inexorabletash/a279f03ab5610817c0540c83857e4295
  *)
  module Fts = struct
    let object_store_name = Jstr.v "fts"

    open Indexeddb

    let on_version_change db =
      let fts = Database.create_object_store db object_store_name in

      let _terms_index =
        ObjectStore.create_index fts
          ~key_path:Jv.(of_string "terms")
          ~object_parameters:Jv.(obj [| ("multiEntry", true') |])
        @@ Jstr.v "fts_terms"
      in

      ()

    let object_store tx = Transaction.object_store tx object_store_name

    (* Returns list of stemmed terms that appear in `text` *)
    let get_terms text =
      let segments =
        Uuseg_string.fold_utf_8 `Word (fun segs s -> List.cons s segs) [] text
      in

      List.filter_map
        (fun segment ->
          (* The Stemmer is buggy! *)
          try Some (Stemmer.stem segment) with Invalid_argument _ -> None)
        segments

    let put db ?(tx = rw_tx db) literal key =
      (* Check if Literal datatype is text/string *)
      let datatype = Rdf.Literal.datatype literal in
      let is_text_type =
        Rdf.Iri.equal datatype (Rdf.Namespace.xsd "string")
        || Rdf.Iri.equal datatype (Rdf.Namespace.rdf "langString")
      in

      if is_text_type then
        let fts = object_store tx in
        let terms = get_terms (Rdf.Literal.canonical literal) in
        (* Log.debug (fun m ->
         *     m "Fts.put %a (terms: %a)" Rdf.Literal.pp literal
         *       Fmt.(list string)
         *       terms); *)
        ObjectStore.put fts ~key:(Jv.of_int key)
          Jv.(obj [| ("terms", of_list of_string terms) |])
        >|= ignore
      else return_unit

    let merge_join ?(compare = compare) seqs () =
      let rec forward_node_to boundary node =
        match node with
        | Lwt_seq.Cons (v, rest) ->
            if compare boundary v <= 0 then return node
            else rest () >>= fun next_node -> forward_node_to boundary next_node
        | Lwt_seq.Nil -> return node
      in

      let iter_node node =
        match node with
        | Lwt_seq.Cons (_, rest) -> rest ()
        | Lwt_seq.Nil -> return Lwt_seq.Nil
      in

      let find_boundary nodes =
        List.fold_left
          (fun (at_end, max_opt) node ->
            match (at_end, max_opt, node) with
            | true, _, _ -> (true, None)
            | false, _, Lwt_seq.Nil -> (true, None)
            | false, Some max, Lwt_seq.Cons (v, _) ->
                if compare max v <= 0 then (false, Some v) else (false, Some max)
            | false, None, Lwt_seq.Cons (v, _) -> (false, Some v))
          (false, None) nodes
        |> snd
      in

      let rec iter nodes =
        let boundary_opt = find_boundary nodes in
        match boundary_opt with
        | None -> return Lwt_seq.Nil
        | Some boundary ->
            let* forwarded_nodes =
              Lwt_list.map_p (forward_node_to boundary) nodes
            in
            let forwarded_boundary = find_boundary forwarded_nodes in
            if Option.compare compare forwarded_boundary boundary_opt = 0 then
              let* next_nodes = Lwt_list.map_p iter_node nodes in
              return @@ Lwt_seq.Cons (boundary, fun () -> iter next_nodes)
            else iter forwarded_nodes
      in

      let* init_nodes = Lwt_list.map_p (fun seq -> seq ()) seqs in

      iter init_nodes

    (* [search tx query] retuns a sequence of term ids (as stored in dictionary) of literals that match with the query. *)
    let search tx query : int Lwt_seq.t =
      let terms = get_terms query in

      Log.debug (fun m ->
          m "Fts.search: terms=%a"
            Fmt.(brackets @@ list ~sep:comma string)
            terms);

      ignore tx;

      let fts = object_store tx in
      let fts_terms = ObjectStore.index fts (Jstr.v "fts_terms") in

      (* merge join over the fts_terms index with all terms appearing in query *)
      merge_join ~compare:Int.compare
        (List.map
           (fun term ->
             Index.open_cursor fts_terms (Jv.of_string term)
             |> Cursor.opt_lwt_to_seq
             |> Lwt_seq.map (fun cursor ->
                    Jv.to_int @@ Cursor.primary_key cursor))
           terms)
  end

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
        ObjectStore.create_index dictionary
          ~key_path:Jv.(of_list of_string [ "term" ])
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
        (-30, geo "lat");
        (-31, geo "long");
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

      choice
        [
          iri_parser >>| Rdf.Term.of_iri;
          bnode_parser >>| Rdf.Term.of_blank_node;
          literal_parser >>| Rdf.Term.of_literal;
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

          let* () =
            (* If term is a literal, add entry in fts index *)
            match Rdf.Term.to_literal term with
            | Some literal -> Fts.put db ~tx literal key
            | None -> return ()
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
        ObjectStore.create_index triples
          ~key_path:Jv.(of_list of_string [ "s"; "p"; "o" ])
          ~object_parameters:Jv.(obj [| ("unique", true') |])
        @@ Jstr.v "spo"
      in

      (* Create the s index *)
      let _s_index =
        ObjectStore.create_index triples
          ~key_path:Jv.(of_list of_string [ "s" ])
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "s"
      in

      (* Create the p index *)
      let _p_index =
        ObjectStore.create_index triples
          ~key_path:Jv.(of_list of_string [ "p" ])
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "p"
      in

      (* Create the o index *)
      let _o_index =
        ObjectStore.create_index triples
          ~key_path:Jv.(of_list of_string [ "o" ])
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "o"
      in

      (* Create the sp index *)
      let _sp_index =
        ObjectStore.create_index triples
          ~key_path:Jv.(of_list of_string [ "s"; "p" ])
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "sp"
      in

      (* Create the so index *)
      let _so_index =
        ObjectStore.create_index triples
          ~key_path:Jv.(of_list of_string [ "s"; "o" ])
          ~object_parameters:Jv.(obj [| ("unique", false') |])
        @@ Jstr.v "so"
      in

      (* Create the po index *)
      let _po_index =
        ObjectStore.create_index triples
          ~key_path:Jv.(of_list of_string [ "p"; "o" ])
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
          Log.debug (fun m -> m "Creating fts ObjectStore.");
          Fts.on_version_change db;
          Log.debug (fun m -> m "Creating dictionary ObjectStore.");
          Dictionary.on_version_change db;
          Log.debug (fun m -> m "Creating triples ObjectStore.");
          Triples.on_version_change db)
        (Jstr.v geopub_database_name)
    in

    (* Force update of triple store *)
    updated ();

    return db

  let triple_count db =
    let tx = ro_tx db in
    Indexeddb.ObjectStore.count (Triples.object_store tx) Jv.undefined

  let add_graph db (graph : Rdf.Graph.t) =
    let* () =
      Rdf.Graph.to_triples graph |> List.of_seq
      |> Lwt_list.iter_p (fun triple -> Triples.put db triple >|= ignore)
    in
    return @@ updated ()
end

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
            string "lat" *> (constant @@ geo "lat");
            string "long" *> (constant @@ geo "long");
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
    | "triples", [ None; None; None ] ->
        Log.warn (fun m -> m "EDB: getting all triples");
        ObjectStore.open_cursor triples Jv.undefined |> triples_of_cursor
    | "triples", [ Some (Term s); None; None ] ->
        (* Log.debug (fun m -> m "EDB: using s index"); *)
        let s_index = ObjectStore.index triples (Jstr.v "s") in
        Index.open_cursor s_index (jv_of_index [ s ]) |> triples_of_cursor
    | "triples", [ None; Some (Term p); None ] ->
        (* Log.debug (fun m -> m "EDB: using p index"); *)
        let p_index = ObjectStore.index triples (Jstr.v "p") in
        Index.open_cursor p_index (jv_of_index [ p ]) |> triples_of_cursor
    | "triples", [ None; None; Some (Term o) ] ->
        (* Log.debug (fun m -> m "EDB: using o index"); *)
        let o_index = ObjectStore.index triples (Jstr.v "o") in
        Index.open_cursor o_index (jv_of_index [ o ]) |> triples_of_cursor
    | "triples", [ Some (Term s); Some (Term p); None ] ->
        (* Log.debug (fun m -> m "EDB: using sp index"); *)
        let sp_index = ObjectStore.index triples (Jstr.v "sp") in
        Index.open_cursor sp_index (jv_of_index [ s; p ]) |> triples_of_cursor
    | "triples", [ Some (Term s); None; Some (Term o) ] ->
        (* Log.debug (fun m -> m "EDB: using so index"); *)
        let so_index = ObjectStore.index triples (Jstr.v "so") in
        Index.open_cursor so_index (jv_of_index [ s; o ]) |> triples_of_cursor
    | "triples", [ None; Some (Term p); Some (Term o) ] ->
        (* Log.debug (fun m -> m "EDB: using po index"); *)
        let po_index = ObjectStore.index triples (Jstr.v "po") in
        Index.open_cursor po_index (jv_of_index [ p; o ]) |> triples_of_cursor
    | "triples", [ Some (Term s); Some (Term p); Some (Term o) ] ->
        (* Log.debug (fun m -> m "EDB: using spo index"); *)
        let spo_index = ObjectStore.index triples (Jstr.v "spo") in
        Index.open_cursor spo_index (jv_of_index [ s; p; o ])
        |> triples_of_cursor
    | "ftsstore", [ Some (String s); None ] ->
        Store.Fts.search tx s
        |> Lwt_seq.map (fun id ->
               Brr.Console.log [ Jv.of_string "ftsstore:"; Jv.of_int id ];
               id)
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
    (* It's kind of stupid that Datalogl does not allow directly
       querying of EDB predicates ...*)
    {datalog|
    rdf(?s,?p,?o) :- triples(?s,?p,?o).
    fts(?query, ?term) :- ftsstore(?query, ?term). 
    withgeo(?s) :- triples(?s, lat, ?lat), triples(?s, long, ?lng).
      |datalog}
    ^ rhodf
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
          Atom.make "rdf"
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
          Atom.make "rdf"
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
  query_string db {query|withgeo(?s)|query}
  |> Lwt_seq.return_lwt
  |> Lwt_seq.flat_map (fun set ->
         Lwt_seq.of_seq @@ Datalog.Tuple.Set.to_seq set)
  |> Lwt_seq.filter_map_s (function
       | [ Datalog.Term term_id ] -> Store.Dictionary.get db term_id
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

let delete db =
  Log.info (fun m -> m "Deleting IndexedDB databse.");
  Indexeddb.(
    return @@ Database.close db >>= fun () ->
    Database.delete (Jstr.v geopub_database_name))

let () = Datalog.set_debug true

let test_datalog db =
  let q =
    Datalog.(
      Atom.make "fts"
        Term.[ make_constant @@ String "something"; make_variable "term" ])
  in
  let* tuples = query db q in

  (* let* () =
   *   Store.Fts.search (Store.ro_tx db) "you"
   *   |> Lwt_seq.fold_left (fun () id -> Brr.Console.log [ Jv.of_int id ]) ()
   * in *)
  return
  @@ Log.debug (fun m -> m "test_datalog: %a" Datalog.Tuple.Set.pp tuples)
