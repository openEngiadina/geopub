(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* A Datalog instance that is exposed for querying. The difference to
   the one used for evaluation is that RDF Terms can be used directly,
   instead of integer identifiers.*)

open Lwt
open Lwt.Syntax

module Constant = struct
  type t =
    | Rdf of Rdf.Term.t
    | FtsQuery of string
    | GeoQuery of (float * float * int)

  let compare = compare

  let iri_parser =
    Angstrom.(
      char '<'
      *> (many_till any_char (char '>') >>| List.to_seq >>| String.of_seq)
      >>| Rdf.Iri.of_string)

  let string_parser =
    Angstrom.(
      char '"'
      *> (many_till any_char (char '"') >>| List.to_seq >>| String.of_seq))

  let geo_parser =
    let to_float chars =
      chars |> List.to_seq |> String.of_seq |> Float.of_string_opt |> function
      | Some float -> Angstrom.return float
      | None -> Angstrom.fail "could not parser float in GeoHash query"
    in
    let to_int chars =
      chars |> List.to_seq |> String.of_seq |> int_of_string_opt |> function
      | Some int -> Angstrom.return int
      | None -> Angstrom.fail "could not parser integer in GeoHash query"
    in

    Angstrom.(
      (fun _ lat long precision -> GeoQuery (lat, long, precision))
      <$> string "GeoHash("
      <*> (many_till any_char (char ',') >>= to_float)
      <*> (many_till any_char (char ',') >>= to_float)
      <*> (many_till any_char (char ')') >>= to_int))

  let parser =
    Angstrom.(
      choice ~failure_msg:"not a valid RDF term"
        [
          string "type"
          *> (return @@ Rdf (Rdf.Term.of_iri @@ Rdf.Namespace.rdf "type"));
          (iri_parser >>| fun iri -> Rdf (Rdf.Term.of_iri iri));
          (string_parser >>| fun s -> FtsQuery s);
          geo_parser;
        ])

  let pp ppf t =
    match t with
    | Rdf t -> Fmt.pf ppf "%a" Rdf.Term.pp t
    | FtsQuery s -> Fmt.pf ppf "\"%s\"" s
    | GeoQuery (lat, long, precision) ->
        Fmt.pf ppf "%a" Datalog.Constant.pp
          (Datalog.Constant.GeoQuery (lat, long, precision))
end

module EDatalog = Datalog
include Datalogl.Make (Constant)

module Dictionary = struct
  let lookup_constant db ?tx c =
    match c with
    | Constant.Rdf term ->
        let* id =
          Store.Dictionary.lookup db ?tx term >|= Option.value ~default:0
        in
        return @@ EDatalog.Constant.Rdf id
    | Constant.FtsQuery q -> return @@ EDatalog.Constant.FtsQuery q
    | Constant.GeoQuery (lat, long, zoom) ->
        return @@ EDatalog.Constant.GeoQuery (lat, long, zoom)

  let lookup_term db ?tx =
    Term.map
      (fun v -> return @@ EDatalog.Term.make_variable v)
      (fun c -> lookup_constant db ?tx c >|= EDatalog.Term.make_constant)

  let lookup_atom db ?tx atom =
    let predicate_symbol = Atom.predicate atom in
    let* eterms = Lwt_list.map_s (lookup_term db ?tx) @@ Atom.terms atom in
    return @@ EDatalog.Atom.make predicate_symbol eterms

  let lookup_literal db ?tx literal =
    let predicate_symbol = Literal.predicate literal in
    let* eterms =
      Lwt_list.map_s (lookup_term db ?tx) @@ Literal.terms literal
    in
    if Literal.is_positive literal then
      return
      @@ EDatalog.(Literal.make_positive @@ Atom.make predicate_symbol eterms)
    else
      return
      @@ EDatalog.(Literal.make_negative @@ Atom.make predicate_symbol eterms)

  let lookup_clause db ?tx clause =
    let head = Clause.head clause in
    let* ehead = lookup_atom db ?tx head in
    let literals = Clause.body clause in
    let* eliterals =
      Literal.Set.to_seq literals
      |> List.of_seq
      |> Lwt_list.map_s (lookup_literal db ?tx)
      >|= EDatalog.Literal.Set.of_list
    in
    return @@ EDatalog.Clause.make ehead eliterals

  let get_constant db ?tx ec =
    match ec with
    | EDatalog.Constant.Rdf id ->
        let* term = Store.Dictionary.get db ?tx id >|= Option.get in
        return @@ Constant.Rdf term
    | EDatalog.Constant.FtsQuery q -> return @@ Constant.FtsQuery q
    | EDatalog.Constant.GeoQuery (lat, long, zoom) ->
        return @@ Constant.GeoQuery (lat, long, zoom)

  let get_tuple db ?tx et = Lwt_list.map_p (get_constant db ?tx) et

  let get_tuple_set db ?tx ets =
    EDatalog.Tuple.Set.to_seq ets
    |> Lwt_seq.of_seq
    |> Lwt_seq.map_s (get_tuple db ?tx)
    |> Lwt_seq.fold_left
         (fun set tuple -> Tuple.Set.add tuple set)
         Tuple.Set.empty
end
