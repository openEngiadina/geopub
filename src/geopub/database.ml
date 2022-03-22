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
  (** An encoding of RDF terms to Javascript values inspired by the RDF/JSON serialization. *)

  let to_jv term =
    let s = Rdf_cbor.encode_term term in
    let array = Tarray.(create Uint8 @@ String.length s) in
    Tarray.iter (fun i _ -> Tarray.set array i (String.get_uint8 s i)) array;
    array |> Tarray.buffer |> Tarray.Buffer.to_jv
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
  Brr.Console.log [ tx ];
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

  Brr.Console.log [ triples ];
  return_unit

let add_rdf db graph =
  let* () = inspect db in
  let tx =
    Transaction.create db ~mode:Transaction.ReadWrite
      [ triples_object_store_name ]
  in
  Brr.Console.log [ tx ];
  let triples = Transaction.object_store tx triples_object_store_name in
  let* () =
    Rdf.Graph.to_triples graph |> List.of_seq
    |> Lwt_list.iter_p (fun triple ->
           Console.log [ Triple.to_jv triple ];
           let* id = ObjectStore.put triples (Triple.to_jv triple) in
           Brr.Console.log [ id ];
           return_unit)
  in
  return @@ Transaction.commit tx
