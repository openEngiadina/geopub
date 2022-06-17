(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(** The IndexedDB backed storage *)

(** The Store is implemented using following IndexedDB ObjectStores:

- Fts: A full-text search index that maps stemmed terms to literals.
- Dictionary: Stores a mapping of RDF Terms to integer keys (auto incremented).
- Triples: Stores triples using keys of terms as stored in Dictionary.

   *)

open Lwt
open Lwt.Syntax
open Lwt_react

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Database.Store"

module Log = (val Logs.src_log src : Logs.LOG)

(* Constants *)

let geopub_database_version = 4
let geopub_database_name = "GeoPub"

(* Event that signals store updates *)
let on_update, updated = E.create ()

(* Helpers for creating IndexedDB transactions *)

let ro_tx db =
  Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadOnly
    [ Jstr.v "dictionary"; Jstr.v "triples"; Jstr.v "fts"; Jstr.v "geo" ]

let rw_tx db =
  Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadWrite
    [ Jstr.v "dictionary"; Jstr.v "triples"; Jstr.v "fts"; Jstr.v "geo" ]

module Fts = struct
  (** The Full-Text-Search index

     This maintains an index from stemmed terms to literals.

     The primary key of entries is the id as stored in the dictionary for the term.

     Inspiration comes from this gist: https://gist.github.com/inexorabletash/a279f03ab5610817c0540c83857e4295
  *)
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
        m "Fts.search: terms=%a" Fmt.(brackets @@ list ~sep:comma string) terms);

    ignore tx;

    let fts = object_store tx in
    let fts_terms = ObjectStore.index fts (Jstr.v "fts_terms") in

    (* merge join over the fts_terms index with all terms appearing in query *)
    merge_join ~compare:Int.compare
      (List.map
         (fun term ->
           Index.open_cursor fts_terms (KeyRange.only @@ Jv.of_string term)
           |> Cursor.opt_lwt_to_seq
           |> Lwt_seq.map (fun cursor -> Jv.to_int @@ Cursor.primary_key cursor))
         terms)
end

module Dictionary = struct
  (** The Dictionary store maintains a mapping of RDF Terms to integer ids.

This improves performance as serialized RDF Terms are only stored once
and then referred to by lighter integer indexes.
 *)
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
    let geo =
      Rdf.Namespace.make_namespace "http://www.w3.org/2003/01/geo/wgs84_pos#"
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
      (-32, geo "SpatialThing");
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
    Angstrom.parse_string ~consume:Angstrom.Consume.All parser (Jv.to_string jv)
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

module Geo = struct
  (** The Geospatial index

    This maintains an index from the Geohash to terms that have some geospatial information attached. *)

  open Indexeddb

  let object_store_name = Jstr.v "geo"

  let on_version_change db =
    let geo = Database.create_object_store db object_store_name in

    let _geohash_index =
      ObjectStore.create_index geo
        ~key_path:Jv.(of_string "geohash")
        ~object_parameters:Jv.(obj [| ("multiEntry", true') |])
      @@ Jstr.v "geohash"
    in

    ()

  let object_store tx = Transaction.object_store tx object_store_name

  module IntMap = Map.Make (Int)

  let geo_lang_long_of_term object' =
    match Rdf.Triple.Object.to_literal object' with
    | Some literal -> Float.of_string_opt @@ Rdf.Literal.canonical literal
    | None -> None

  let put tx seq =
    (* Adds sequence of triples to the geo index. As some geo
       information depends on multiple triples we need to process the
       entire sequence and piece together the information *)
    let lat_map = ref IntMap.empty in
    let long_map = ref IntMap.empty in
    let geo = object_store tx in

    Lwt_seq.append
      (Lwt_seq.map_s
         (fun (s_id, p_id, object') ->
           if p_id = -30 then
             (* geo:lat *)
             match geo_lang_long_of_term object' with
             | Some lat ->
                 lat_map := IntMap.add s_id lat !lat_map;
                 return (s_id, p_id, object')
             | None -> return (s_id, p_id, object')
           else if p_id = -31 then
             (* geo:long *)
             match geo_lang_long_of_term object' with
             | Some long ->
                 long_map := IntMap.add s_id long !long_map;
                 return (s_id, p_id, object')
             | None -> return (s_id, p_id, object')
           else return (s_id, p_id, object'))
         seq)
      (* Add a empty Seq at the end. Even if it is empty it will be
         evaluated. Smells like finalizers of transducers ... *)
        (fun () ->
        let* () =
          IntMap.merge
            (fun _s_id lat_opt long_opt ->
              match (lat_opt, long_opt) with
              | Some lat, Some long ->
                  Some (Geohash.encode ~precision:10 (lat, long))
              | _ -> None)
            !lat_map !long_map
          |> IntMap.to_seq |> Lwt_seq.of_seq
          |> Lwt_seq.iter_s (fun (s_id, geohash) ->
                 ObjectStore.put geo ~key:(Jv.of_int s_id)
                   Jv.(obj [| ("geohash", of_list of_string [ geohash ]) |])
                 >|= ignore)
        in
        return Lwt_seq.Nil)

  let search tx (lat, long, precision) =
    let geohash_query = Geohash.encode ~precision (lat, long) in

    Log.debug (fun m ->
        m "Geo.search: (%f, %f, %d), GeoHash: %s" lat long precision
          geohash_query);

    let geo = object_store tx in
    let geohash_index = ObjectStore.index geo (Jstr.v "geohash") in

    Index.open_cursor geohash_index
      (KeyRange.lower_bound @@ Jv.of_string geohash_query)
    |> Cursor.opt_lwt_to_seq
    |> Lwt_seq.map (fun cursor -> Jv.to_int @@ Cursor.primary_key cursor)
end

(* The Triple Store *)

module Triples = struct
  (** The Triple store maintains indexes for RDF triples.

RDF terms are stored using integer identifiers as stored in the `Dictionary`.*)

  let object_store_name = Jstr.v "triples"

  open Indexeddb

  let object_store tx = Indexeddb.Transaction.object_store tx object_store_name

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

  let put_graph db (graph : Rdf.Graph.t) =
    let tx = rw_tx db in

    Rdf.Graph.to_triples graph |> Lwt_seq.of_seq
    |> Lwt_seq.filter_map_s (fun (triple : Rdf.Triple.t) ->
           let* s_id =
             triple.subject |> Rdf.Triple.Subject.to_term
             |> Dictionary.put db ~tx
           in
           let* p_id =
             triple.predicate |> Rdf.Triple.Predicate.to_term
             |> Dictionary.put db ~tx
           in
           let* o_id =
             triple.object' |> Rdf.Triple.Object.to_term
             |> Dictionary.put db ~tx
           in
           let triples = object_store tx in

           let spo_index = ObjectStore.index triples (Jstr.v "spo") in

           let* key_opt =
             Index.get_key spo_index (Jv.of_list Jv.of_int [ s_id; p_id; o_id ])
             >|= Jv.to_option Jv.to_int
           in

           match key_opt with
           | None ->
               let _key =
                 ObjectStore.put triples (jv_of_triple s_id p_id o_id)
               in

               (* return the triple for further indexing (use subject
                  and predicate ids instead of real terms) *)
               return_some (s_id, p_id, triple.object')
           | Some _key -> return_none)
    |> Geo.put tx |> Lwt_seq.iter ignore
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
        Log.debug (fun m -> m "Creating geo ObjectStore.");
        Geo.on_version_change db;
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

let delete db =
  Log.info (fun m -> m "Deleting IndexedDB databse.");
  Indexeddb.(
    return @@ Database.close db >>= fun () ->
    Database.delete (Jstr.v geopub_database_name))

let triple_count db =
  let tx = ro_tx db in
  Indexeddb.ObjectStore.count (Triples.object_store tx) Jv.undefined

let add_graph db (graph : Rdf.Graph.t) = Triples.put_graph db graph >|= updated
