(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

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
    String.init (Tarray.length array) (fun i -> Char.chr @@ Tarray.get array i)
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
