(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)
open Indexeddb
open Brr
open Lwt
open Lwt.Syntax

let src = Logs.Src.create "GeoPub.Database"

module Log = (val Logs.src_log src : Logs.LOG)

(* TODO Improve database schema

   Currently triples are encoded with terms in-place. Terms that occur
   multiple times use up unnecessary space. It would be much better to
   have an ObjectStore that stores term encodings and triples only hold
   the primary key to the terms. A bit like the dictionary in HDT.

   Use a cache for improving decoding of terms (e.g. with
   https://gitlab.com/nomadic-labs/ringo).
*)

let geopub_database_version = 1
let geopub_database_name = "GeoPub"
let triples_object_store_name = Jstr.v "triples"

type t = Database.t

let init () =
  (* let* () = Database.delete @@ Jstr.v geopub_database_name in *)
  Database.open' ~version:geopub_database_version
    ~on_version_change:(fun db ->
      let open Database.VersionChange in
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
  |> Lwt_result.catch

(* Encoding *)

module IDBEncoding = struct
  (* Javascript Encoding for IndexedDB *)

  (* RDF terms are encoded using the binary (and experimental) RDF/CBOR encoding.

     This seems to be necessary as IndexedDB can not use complex object
     values as keys. I.e. we can not use the RDF/JSON encoding that encodes
     terms to Javascript objects with `type` and `value` fields.

     The CBOR encoding may be more compact and thus efficient. *)

  let jv_of_term term =
    Rdf_cbor.encode_term term
    (* Quite a round-about way of creating a Javascript
       Buffer. However this seems to be as good as it gets. OCaml strings
       live on the heap whereas Javascript Buffers are in externally
       allocated memory. No way to transform without copying. *)
    |> String.to_seq
    |> Seq.map Char.code |> Array.of_seq
    |> Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout
    |> Tarray.of_bigarray1 |> Tarray.buffer |> Tarray.Buffer.to_jv

  let term_of_jv jv =
    let array = Tarray.Buffer.of_jv jv |> Tarray.of_buffer Tarray.Uint8 in
    let s =
      String.init (Tarray.length array) (fun i ->
          Char.chr @@ Tarray.get array i)
    in
    Angstrom.parse_string ~consume:Angstrom.Consume.All Rdf_cbor.Parser.term s

  let jv_of_terms terms = Jv.of_list jv_of_term terms

  let jv_of_triple (triple : Rdf.Triple.t) =
    Jv.obj
      [|
        ("s", jv_of_term @@ Rdf.Triple.Subject.to_term triple.subject);
        ("p", jv_of_term @@ Rdf.Triple.Predicate.to_term triple.predicate);
        ("o", jv_of_term @@ Rdf.Triple.Object.to_term triple.object');
      |]

  let triple_of_jv_exn jv =
    let s = Jv.get jv "s" |> term_of_jv |> Result.get_ok in
    let p = Jv.get jv "p" |> term_of_jv |> Result.get_ok in
    let o = Jv.get jv "o" |> term_of_jv |> Result.get_ok in
    Rdf.Triple.(
      make
        (Rdf.Term.map s Subject.of_iri Subject.of_blank_node (fun _ ->
             failwith "subject can not be literal"))
        (Rdf.Term.map p Predicate.of_iri
           (fun _ -> failwith "predicate can not be blank node")
           (fun _ -> failwith "predicate can not be literal"))
        (Object.of_term o))

  let tuple_of_jv_exn jv =
    let s = Jv.get jv "s" |> term_of_jv |> Result.get_ok in
    let p = Jv.get jv "p" |> term_of_jv |> Result.get_ok in
    let o = Jv.get jv "o" |> term_of_jv |> Result.get_ok in
    [ s; p; o ]
end

module Datalog = Datalogl.Make (struct
  type t = Rdf.Term.t

  let compare = Rdf.Term.compare
  let parser = Angstrom.fail "Can not parse RDF terms from Datalog queries yet."
  let pp = Rdf.Term.pp
end)

let datalog_database tx predicate pattern =
  let stream_of_list l_p =
    (* TODO instead of transforming list to Lwt_stream we should use IDBCursor. *)
    let stream, push, set_reference = Lwt_stream.create_with_reference () in
    let pusher =
      l_p
      >|= List.iter (fun el ->
              let tuple = IDBEncoding.tuple_of_jv_exn el in
              push (Some tuple))
      (* Close stream after all elements are pushed *)
      >|= fun () -> push None
    in
    (* Don't forget the pusher *)
    set_reference pusher;
    stream
  in
  (* Open the triples object store *)
  let triples = Transaction.object_store tx triples_object_store_name in
  (* Get triples with index matching the query pattern *)
  match (predicate, pattern) with
  | "rdf_db", [ None; None; None ] ->
      ObjectStore.get_all triples Jv.undefined |> stream_of_list
  | "rdf_db", [ Some s; None; None ] ->
      let s_index = ObjectStore.index triples (Jstr.v "s") in
      Index.get_all s_index IDBEncoding.(jv_of_terms [ s ]) |> stream_of_list
  | "rdf_db", [ None; Some p; None ] ->
      let p_index = ObjectStore.index triples (Jstr.v "p") in
      Index.get_all p_index IDBEncoding.(jv_of_terms [ p ]) |> stream_of_list
  | "rdf_db", [ None; None; Some o ] ->
      let o_index = ObjectStore.index triples (Jstr.v "o") in
      Index.get_all o_index IDBEncoding.(jv_of_terms [ o ]) |> stream_of_list
  | "rdf_db", [ Some s; Some p; None ] ->
      let sp_index = ObjectStore.index triples (Jstr.v "sp") in
      Index.get_all sp_index IDBEncoding.(jv_of_terms [ s; p ])
      |> stream_of_list
  | "rdf_db", [ Some s; None; Some o ] ->
      let so_index = ObjectStore.index triples (Jstr.v "so") in
      Index.get_all so_index IDBEncoding.(jv_of_terms [ s; o ])
      |> stream_of_list
  | "rdf_db", [ None; Some p; Some o ] ->
      let po_index = ObjectStore.index triples (Jstr.v "po") in
      Index.get_all po_index IDBEncoding.(jv_of_terms [ p; o ])
      |> stream_of_list
  | "rdf_db", [ Some s; Some p; Some o ] ->
      let spo_index = ObjectStore.index triples (Jstr.v "spo") in
      Index.get_all spo_index IDBEncoding.(jv_of_terms [ s; p; o ])
      |> stream_of_list
  | _, _ -> Lwt_stream.of_list []

let inspect db =
  let tx =
    Transaction.create db ~mode:Transaction.ReadOnly
      [ triples_object_store_name ]
  in

  let query =
    Datalog.(
      Atom.make "rdf"
        [
          Term.make_variable "s"; Term.make_variable "p"; Term.make_variable "o";
        ])
  in

  let program =
    Datalog.(
      Program.empty
      |> Program.add
           (Clause.make
              (Atom.make "rdf"
                 [
                   Term.make_variable "s";
                   Term.make_variable "p";
                   Term.make_variable "o";
                 ])
              [
                Literal.make_positive
                @@ Atom.make "rdf_db"
                     [
                       Term.make_variable "s";
                       Term.make_variable "p";
                       Term.make_variable "o";
                     ];
              ]))
  in

  let* tuples = Datalog.query ~database:(datalog_database tx) ~program query in

  Log.debug (fun m -> m "inspect: %a" Datalog.Tuple.Set.pp tuples);
  return_unit

let add_triple tx (triple : Rdf.Triple.t) =
  let triples = Transaction.object_store tx triples_object_store_name in
  let spo = ObjectStore.index triples (Jstr.v "spo") in
  let* count =
    Index.count spo
      (IDBEncoding.jv_of_terms
         [
           Rdf.Triple.Subject.to_term triple.subject;
           Rdf.Triple.Predicate.to_term triple.predicate;
           Rdf.Triple.Object.to_term triple.object';
         ])
  in
  if count = 0 then
    (* TODO usage of jv_of_triple is inefficient as we have already encoded triples into Buffers above. This should be reused. *)
    let* _ = ObjectStore.add triples (IDBEncoding.jv_of_triple triple) in
    return_unit
  else return_unit

let add_rdf db graph =
  let* () = inspect db in
  let tx =
    Transaction.create db ~mode:Transaction.ReadWrite
      [ triples_object_store_name ]
  in
  let* () =
    Rdf.Graph.to_triples graph |> List.of_seq |> Lwt_list.iter_p (add_triple tx)
  in
  return @@ Transaction.commit tx
