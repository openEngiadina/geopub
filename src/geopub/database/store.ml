(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(** The IndexedDB backed storage *)

(** The Store is implemented using following IndexedDB ObjectStores:

- Dictionary: Stores a mapping of RDF Terms to integer keys (auto incremented).
- Fts: A full-text search index that maps stemmed terms to literals.
- Geo: A geo-spatial index that maps Geohash identifiers to subjects.
- Triples: Stores triples using keys of terms as stored in Dictionary.

   *)

open Lwt
open Lwt.Syntax
open Lwt_react

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Database.Store"

module Log = (val Logs.src_log src : Logs.LOG)

(* Constants *)

let geopub_database_version = 6
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

  let on_version_change db =
    let fts = Indexeddb.Database.create_object_store db object_store_name in

    let _terms_index =
      Indexeddb.ObjectStore.create_index fts
        ~key_path:Jv.(of_string "terms")
        ~object_parameters:Jv.(obj [| ("multiEntry", true') |])
      @@ Jstr.v "fts_terms"
    in

    ()

  let object_store tx = Indexeddb.Transaction.object_store tx object_store_name

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
      Indexeddb.ObjectStore.put fts ~key:(Jv.of_int key)
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
    let fts_terms = Indexeddb.ObjectStore.index fts (Jstr.v "fts_terms") in

    (* merge join over the fts_terms index with all terms appearing in query *)
    merge_join ~compare:Int.compare
      Indexeddb.(
        List.map
          (fun term ->
            Index.open_cursor fts_terms (KeyRange.only @@ Jv.of_string term)
            |> Cursor.opt_lwt_to_seq
            |> Lwt_seq.map (fun cursor ->
                   Jv.to_int @@ Cursor.primary_key cursor))
          terms)
end

module Term = struct
  (** Encoding of RDF terms using RDF/CBOR *)

  open Brr

  (** Returns a JavaScript ArrayBuffer that holds the encoded term. *)
  let to_jv term =
    Rdf_cbor.Term.encode term |> Cborl.write |> Seq.map Char.code
    |> Array.of_seq
    |> Tarray.(of_int_array Uint8)
    |> Tarray.buffer |> Tarray.Buffer.to_jv

  let to_jv_obj term = Jv.obj [| ("term", to_jv term) |]

  let of_jv jv =
    let tarray = jv |> Tarray.Buffer.of_jv |> Tarray.(of_buffer Uint8) in
    let len = Tarray.length tarray in
    let i = ref 0 in
    let read () =
      if !i < len then (
        i := !i + 1;
        Some (Tarray.get tarray (!i - 1)))
      else None
    in
    Seq.of_dispenser read |> Seq.map Char.chr |> Cborl.read
    |> Rdf_cbor.Term.decode

  let of_jv_obj jv = of_jv (Jv.get jv "term")
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
      Indexeddb.Database.create_object_store db
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
    let geosparql =
      Rdf.Namespace.make_namespace "http://www.opengis.net/ont/geosparql#"
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
      (-40, geosparql "hasGeometry");
      (-50, Rdf.Iri.of_string "https://www.openstreetmap.org/node");
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
    Rdf.Term.map encode_iri encode_blank_node encode_literal term

  (* ObjectStore access *)

  let get db ?(tx = ro_tx db) id =
    match constant_get id with
    | Some term -> return_some term
    | None ->
        let dictionary = object_store tx in
        let* term_jv_opt = ObjectStore.get dictionary (Jv.of_int id) in
        Option.map Term.of_jv_obj term_jv_opt |> return

  let lookup db ?(tx = ro_tx db) term =
    match constant_lookup term with
    | Some id -> return_some id
    | None ->
        let dictionary = object_store tx in
        let term_index = term_index dictionary in
        Index.get_key term_index (Jv.of_list Term.to_jv [ term ])
        >|= Jv.to_option Jv.to_int

  let put db ?(tx = rw_tx db) term =
    let dictionary = object_store tx in
    let* key_opt = lookup db ~tx term in
    match key_opt with
    | None ->
        let* key =
          ObjectStore.put dictionary (Term.to_jv_obj term) >|= Jv.to_int
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
    let geo = Indexeddb.Database.create_object_store db object_store_name in

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

  let wkt_point object' =
    let geosparql =
      Rdf.Namespace.make_namespace "http://www.opengis.net/ont/geosparql#"
    in
    let float_parser =
      let open Angstrom in
      many_till any_char (char ' ' <|> char ')')
      >>| List.to_seq >>| String.of_seq
      >>= fun s ->
      match float_of_string_opt s with
      | Some f -> return f
      | None -> fail "could not parse float in wkt POINT"
    in
    let parser =
      let open Angstrom in
      (* Note lat and long seem to be switched up in WKT points *)
      (fun _ long lat -> (lat, long))
      <$> string "POINT(" <*> float_parser <*> float_parser
    in
    let parse s =
      Angstrom.parse_string ~consume:Angstrom.Consume.All parser s
      |> Result.to_option
    in
    match Rdf.Triple.Object.to_literal object' with
    | Some literal ->
        if Rdf.Iri.equal (Rdf.Literal.datatype literal) (geosparql "wktLiteral")
        then parse (Rdf.Literal.canonical literal)
        else None
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
           else if p_id = -40 then
             match wkt_point object' with
             | Some (lat, long) ->
                 let geohash = Geohash.encode ~precision:10 (lat, long) in
                 ObjectStore.put geo ~key:(Jv.of_int s_id)
                   Jv.(obj [| ("geohash", of_list of_string [ geohash ]) |])
                 >>= fun _ -> return (s_id, p_id, object')
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
      (KeyRange.bound
         (* lower bound is the geohash *)
         (Jv.of_string geohash_query)
         (* upper bound is geohash concatenated with z - the highest possible GeoHash digit *)
         (Jv.of_string @@ geohash_query ^ "z"))
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
      Indexeddb.Database.create_object_store db
        ~options:(Jv.obj [| ("autoIncrement", Jv.true') |])
        object_store_name
    in

    (* Create the spo index *)
    let _spo_index =
      ObjectStore.create_index triples
        ~key_path:Jv.(of_string "spo")
        ~object_parameters:Jv.(obj [| ("unique", true') |])
      @@ Jstr.v "spo"
    in

    (* Create the pos index *)
    let _pos_index =
      ObjectStore.create_index triples
        ~key_path:Jv.(of_string "pos")
        ~object_parameters:Jv.(obj [| ("unique", true') |])
      @@ Jstr.v "pos"
    in

    (* Create the osp index *)
    let _pos_index =
      ObjectStore.create_index triples
        ~key_path:Jv.(of_string "osp")
        ~object_parameters:Jv.(obj [| ("unique", true') |])
      @@ Jstr.v "osp"
    in

    ()

  (* Encoding *)

  let jv_of_triple s p o =
    Jv.obj
      [|
        ("spo", Jv.(of_list of_int [ s; p; o ]));
        ("pos", Jv.(of_list of_int [ p; o; s ]));
        ("osp", Jv.(of_list of_int [ o; s; p ]));
      |]

  let triple_of_jv jv = Jv.(to_list to_int @@ get jv "spo")

  (* ObjectStore access *)

  let deref db ?(tx = ro_tx db) triple_ids =
    let* terms = Lwt_list.map_s (Dictionary.get db ~tx) triple_ids in
    match terms with
    | [ Some s; Some p; Some o ] ->
        return_some
        @@ Rdf.Triple.(
             make
               (Rdf.Term.map Subject.of_iri Subject.of_blank_node
                  (fun _ -> failwith "unexpcted literal")
                  s)
               (Rdf.Term.map Predicate.of_iri
                  (fun _ -> failwith "unexpected blank_node")
                  (fun _ -> failwith "unexpected literal")
                  p)
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
  return @@ Indexeddb.Database.close db >>= fun () ->
  Indexeddb.Database.delete (Jstr.v geopub_database_name)

let triple_count db =
  let tx = ro_tx db in
  Indexeddb.ObjectStore.count (Triples.object_store tx) Jv.undefined

let add_graph db (graph : Rdf.Graph.t) = Triples.put_graph db graph >|= updated
