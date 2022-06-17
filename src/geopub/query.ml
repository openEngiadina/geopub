(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Brr_io
open Lwt
open Lwt.Syntax
open Lwt_react
module Database = Geopub_database
module Datalog = Database.Datalog

let view_constant db = function
  | Datalog.Constant.Rdf id -> (
      let* term = Database.Store.Dictionary.get db id in
      match term with
      | None -> return @@ El.txt' "ERROR: Could not get term for id"
      | Some term ->
          Rdf.Term.map term
            (Ui_rdf.view_pretty_iri db)
            (fun bnode -> return @@ Ui_rdf.view_blank_node bnode)
            (fun literal -> return @@ Ui_rdf.view_literal literal))
  | Datalog.Constant.FtsQuery s -> return @@ El.txt' s
  | Datalog.Constant.GeoQuery _ as geo_query ->
      return @@ El.txt' @@ Format.asprintf "%a" Datalog.Constant.pp geo_query

let view_variable var = return @@ El.txt' ("?" ^ var)

let view_results db query results =
  let* header =
    Datalog.Atom.terms query
    |> Lwt_list.map_s (Datalog.Term.map view_variable (view_constant db))
    >|= List.map (fun term_el -> El.th [ term_el ])
    >|= fun els -> El.tr els
  in
  let* results =
    results |> Datalog.Tuple.Set.to_seq |> Lwt_seq.of_seq
    |> Lwt_seq.map_s (fun tuple ->
           Lwt_list.map_p (view_constant db) tuple
           >|= List.map (fun constant_el -> El.td [ constant_el ])
           >|= fun tuple_els -> El.tr tuple_els)
    |> Lwt_seq.to_list
  in

  return
  @@ El.table ~at:At.[ class' @@ Jstr.v "results-table" ] (header :: results)

module Query = struct
  module Constant = struct
    type t =
      | Iri of Rdf.Iri.t
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
            string "type" *> (return @@ Iri (Rdf.Namespace.rdf "type"));
            (iri_parser >>| fun iri -> Iri iri);
            (string_parser >>| fun s -> FtsQuery s);
            geo_parser;
          ])

    let pp ppf t =
      match t with
      | Iri t -> Fmt.pf ppf "%a" Rdf.Iri.pp t
      | FtsQuery s -> Fmt.pf ppf "\"%s\"" s
      | GeoQuery (lat, long, precision) ->
          Fmt.pf ppf "%a" Datalog.Constant.pp
            (Datalog.Constant.GeoQuery (lat, long, precision))
  end

  (* Instantiate a faux instance of Datalog for parsing *)
  module ParserDatalog = Datalogl.Make (Constant)

  let to_datalog_term db term =
    ParserDatalog.Term.map
      (fun var -> Datalog.Term.make_variable var |> Lwt_result.return)
      (function
        | Constant.Iri iri ->
            Database.Store.Dictionary.lookup db (Rdf.Term.of_iri iri)
            >|= Option.map (fun id ->
                    Datalog.(Term.make_constant @@ Constant.Rdf id))
            >|= Option.to_result ~none:"Could not lookup IRI in dictionary"
        | FtsQuery s ->
            Datalog.(Term.make_constant @@ Constant.FtsQuery s)
            |> Lwt_result.return
        | GeoQuery (lat, long, precision) ->
            Datalog.(
              Term.make_constant @@ Constant.GeoQuery (lat, long, precision))
            |> Lwt_result.return)
      term

  let to_datalog_atom db atom =
    Lwt_list.fold_left_s
      (fun terms term ->
        Lwt_result.both (return terms) (to_datalog_term db term)
        |> Lwt_result.map (fun (terms, term) -> List.append terms [ term ]))
      (Ok [])
      (ParserDatalog.Atom.terms atom)
    |> Lwt_result.map (fun terms ->
           Datalog.Atom.make (ParserDatalog.Atom.predicate atom) terms)

  let parse db query_string =
    Lwt_result.bind
      (return
      @@ Angstrom.parse_string ~consume:Angstrom.Consume.All
           ParserDatalog.Atom.parser query_string)
      (to_datalog_atom db)
end

let query_string_form query_string =
  let query_string_s, update_query_string = S.create query_string in
  let form_el =
    El.(
      form
        ~at:At.[ class' @@ Jstr.v "query-form" ]
        [
          input
            ~at:
              At.
                [
                  type' @@ Jstr.v "search";
                  name @@ Jstr.v "query-string";
                  id @@ Jstr.v "query-string-input";
                  value @@ Jstr.v query_string;
                ]
            ();
          input ~at:At.[ type' @@ Jstr.v "submit"; value @@ Jstr.v "Query" ] ();
        ])
  in

  ( query_string_s,
    Ui.on_el ~default:false Form.Ev.submit
      (fun ev ->
        let form_data =
          Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
        in

        let query_string_value =
          Form.Data.find form_data (Jstr.v "query-string") |> Option.get
        in

        let query_string =
          match query_string_value with
          | `String js -> Jstr.to_string js
          | _ -> failwith "We need better error handling"
        in

        ignore Route.(set_route @@ Route.Query query_string);
        update_query_string query_string)
      form_el )

let help =
  El.(
    div
      [
        p [ txt' "Here you can query the database with Datalog." ];
        p
          [
            txt' "For example this can be used to search for all ";
            a
              ~at:
                At.
                  [
                    href
                    @@ Jstr.v
                         "#query=triple(?s,type,<https://www.w3.org/ns/activitystreams#Note>)";
                  ]
              [ txt' "ActivityStream notes" ];
            txt' ", for all ";
            a
              ~at:
                At.
                  [
                    href
                    @@ Jstr.v
                         "#query=triple-rhodf(?s,type,<https://www.w3.org/ns/activitystreams#Activity>)";
                  ]
              [ txt' "ActivityStreams activities using RDFS type inference" ];
            txt' ", for anything that ";
            a
              ~at:At.[ href @@ Jstr.v "#query=triple-fts(?s,?p,?o, \"Hello\")" ]
              [ txt' "contains the word \"Hello\" (full-text search)" ];
            txt' ", for things ";
            a
              ~at:
                At.
                  [
                    href @@ Jstr.v "#query=geo(GeoHash(46.7965,10.2965,4), ?s)";
                  ]
              [ txt' "in the lower Engadin (geo-spatial search)." ];
          ];
        p
          [
            txt'
              "Try composing your own query below. You can use the pre-defined \
               predicates: triple/3, triple-rhodf/3, triple-fts/4, fts/2 or \
               geo/3. Variables are prefixed with a question mark (\"?\"), \
               IRIs are delimited by angle brackets and strings (for full-text \
               search) with quotation marks. Geo-spatial queries have the \
               form ";
            code [ txt' "GEO(lat,long,precision)" ];
            txt' " where ";
            code [ txt' "precision" ];
            txt' " is the precision in number of GeoHash digits.";
          ];
        p
          [
            txt'
              "Note that this interface does currently not allow defining of \
               your own Datalog program. In the future this will be added to \
               allow the definition of custom predicates that can be queried.";
          ];
      ])

let view (model : Model.t) query_string =
  let query_string_s, query_string_form = query_string_form query_string in

  let* query_s =
    S.map_s (fun q -> Query.parse model.database q) query_string_s
  in

  let* results_s =
    S.map_s
      (function
        | Ok query -> Datalog.query model.database query |> Lwt_result.ok
        | Error msg -> Lwt_result.fail msg)
      query_s
  in

  let* results_table_s =
    S.l2_s
      (fun results query ->
        match (results, query) with
        | Ok results, Ok query -> view_results model.database query results
        | Error msg, _ -> return @@ El.txt' msg
        | _, Error msg -> return @@ El.txt' msg)
      results_s query_s
  in

  let results_table_div = El.div [] in

  let () =
    S.map
      (fun results_table -> El.set_children results_table_div [ results_table ])
      results_table_s
    |> S.keep
  in

  return
  @@ El.(
       div
         ~at:At.[ id @@ Jstr.v "query" ]
         [ h1 [ txt' "Query" ]; help; query_string_form; results_table_div ])
