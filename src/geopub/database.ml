(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)
open Indexeddb
open Brr
open Lwt
open Lwt.Syntax

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

module Term = struct
  (** RDF terms are encoded using the binary (and experimental) RDF/CBOR encoding.

    This seems to be necessary as IndexedDB can not use complex object
    values as keys. I.e. we can not use the RDF/JSON encoding that encodes
    terms to Javascript objects with `type` and `value` fields.

    The CBOR encoding may be more compact and thus efficient. *)

  let to_jv term =
    Rdf_cbor.encode_term term
    (* Quite a round-about way of creating a Javascript
       Buffer. However this seems to be as good as it gets. OCaml strings
       live on the heap whereas Javascript Buffers are in externally
       allocated memory. No way to transform without copying. *)
    |> String.to_seq
    |> Seq.map Char.code |> Array.of_seq
    |> Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout
    |> Tarray.of_bigarray1 |> Tarray.buffer |> Tarray.Buffer.to_jv
end

module Triple = struct
  let to_jv (triple : Rdf.Triple.t) =
    Jv.obj
      [|
        ("s", Term.to_jv @@ Rdf.Triple.Subject.to_term triple.subject);
        ("p", Term.to_jv @@ Rdf.Triple.Predicate.to_term triple.predicate);
        ("o", Term.to_jv @@ Rdf.Triple.Object.to_term triple.object');
      |]
end

let inspect db =
  let tx =
    Transaction.create db ~mode:Transaction.ReadOnly
      [ triples_object_store_name ]
  in
  let triples = Transaction.object_store tx triples_object_store_name in

  let s = ObjectStore.index triples (Jstr.v "s") in

  let* triples =
    Index.get_all s
      (Jv.of_list Term.to_jv
         [
           Rdf.Term.of_iri
           @@ Rdf.Iri.of_string "urn:uuid:669d0fb9-63c8-4c4d-80bf-5380b3aff60d";
         ])
  in

  Console.log [ triples ];
  return_unit

let add_triple tx (triple : Rdf.Triple.t) =
  let triples = Transaction.object_store tx triples_object_store_name in
  let spo = ObjectStore.index triples (Jstr.v "spo") in
  let* count =
    Index.count spo
      (Jv.of_list Term.to_jv
         [
           Rdf.Triple.Subject.to_term triple.subject;
           Rdf.Triple.Predicate.to_term triple.predicate;
           Rdf.Triple.Object.to_term triple.object';
         ])
  in
  if count = 0 then
    let* _ = ObjectStore.add triples (Triple.to_jv triple) in
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
