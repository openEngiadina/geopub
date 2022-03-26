(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* Javascript Encoding of RDF Terms for IndexedDB *)

(* We use a RDF/Turtle inspired encoding into strings. *)

(** Encoding *)

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

let jv_of_term term = string_of_term term |> Jv.of_string

(** Decoding *)

let parser =
  let open Angstrom in
  let iri_parser =
    char '<' *> (many_till any_char (char '>') >>| List.to_seq >>| String.of_seq)
    >>| Rdf.Iri.of_string
  in
  let bnode_parser = fail "we don't parse bnodes" in
  let literal_value_parser =
    char '"' *> (many_till any_char (char '"') >>| List.to_seq >>| String.of_seq)
  in
  let whitespace_lst = [ '\x20'; '\x0a'; '\x0d'; '\x09' ] in
  let char_is_not_equal_to lst d = List.for_all (fun x -> x != d) lst in

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
        (Rdf.Namespace.make_namespace "http://www.w3.org/ns/activitystreams#");
      make_prefix_parser "geo"
        (Rdf.Namespace.make_namespace "http://www.w3.org/2003/01/geo/wgs84_pos#");
    ]

let term_of_jv jv =
  Angstrom.parse_string ~consume:Angstrom.Consume.All parser (Jv.to_string jv)

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
