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
module Datalog = Database.Datalog

let view_constant inspect db = function
  | Datalog.Constant.Rdf term -> Ui_rdf.term inspect db term
  | Datalog.Constant.FtsQuery s -> return @@ El.txt' s
  | Datalog.Constant.GeoQuery _ as geo_query ->
      return @@ El.txt' @@ Format.asprintf "%a" Datalog.Constant.pp geo_query

let view_variable var = return @@ El.txt' ("?" ^ var)

let view_results inspect db query results =
  let* header =
    Datalog.Atom.terms query
    |> Lwt_list.map_s
         (Datalog.Term.map view_variable (view_constant inspect db))
    >|= List.map (fun term_el -> El.th [ term_el ])
    >|= fun els -> El.tr els
  in
  let* results =
    results |> Datalog.Tuple.Set.to_seq |> Lwt_seq.of_seq
    |> Lwt_seq.map_s (fun tuple ->
           Lwt_list.map_p (view_constant inspect db) tuple
           >|= List.map (fun constant_el -> El.td [ constant_el ])
           >|= fun tuple_els -> El.tr tuple_els)
    |> Lwt_seq.to_list
  in

  return
  @@ El.table
       ~at:[ UIKit.table; UIKit.Table.divider; UIKit.Margin.bottom ]
       (header :: results)

let query_string_form query_string =
  let query_string_s, update_query_string = S.create query_string in
  let form_el =
    El.(
      form
        ~at:[ UIKit.Form.stacked; UIKit.margin ]
        [
          textarea
            ~at:
              At.
                [
                  UIKit.Form.input;
                  UIKit.Height.large;
                  name @@ Jstr.v "query-string";
                  id @@ Jstr.v "query-string-input";
                ]
            [ txt' query_string ];
          input
            ~at:
              At.
                [
                  type' @@ Jstr.v "submit";
                  value @@ Jstr.v "Query";
                  UIKit.Button.primary;
                  UIKit.Form.input;
                ]
            ();
        ])
  in

  ( query_string_s,
    Brr_react.Evf.on_el ~default:false Form.Ev.submit
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

        (* ignore Route.(set_route @@ Route.Query query_string); *)
        update_query_string query_string)
      form_el )

let help =
  let example_li query_label query_string =
    El.(
      li
        [ a ~at:[ Route.href (Route.Query query_string) ] [ txt' query_label ] ])
  in

  El.(
    div
      [
        p [ txt' "Here you can query the database with Datalog." ];
        p [ txt' "Some example queries:" ];
        ul
          ~at:[ UIKit.list; UIKit.List.disc; UIKit.margin ]
          [
            example_li "ActivityStreams Notes"
              {datalog|# We define a predicate `note` with a single variable
# This is a clause that defines when the predicate `note` holds:
note(?s) :- triple(?s,type,<https://www.w3.org/ns/activitystreams#Note>).

# We formulate a query - what are the values for ?s such that note(?s) holds:
note(?s)?|datalog};
            example_li
              "ActivityStreams activities using RFDS for type inference"
              {datalog|# In this query we will be able to find all kinds of ActivityStreams activities without explicitly listing all the concrete types (i.e. as:Create, as:Like,...).

# In the machine-readable format of the ActivityStreams specification classes (activities, objects and actors) are defined and are annotated with type information that specify which classes are subclasses of other classes (using the rdfs:subClassOf properties). This can be used to automatically figure out that an resource with type as:Create is also an as:Activity.

# This reasoning is called type inference. In GeoPub we use a set of rules for type inference based on the RDFS schema (https://sci-hub.se/https://doi.org/10.1016/j.websem.2009.07.003).
# The pre-defined Datalog predicate `triple-rhodf` implements exactly these rules and will return all RDF triples including inferred types.

# We can use it to find all resource that are ActivityStreams activities:
activity(?s) :- triple-rhodf(?s, type, <https://www.w3.org/ns/activitystreams#Activity>).

# This predicate is used to find all activities that can displayed in the activity view of this application.

# An interesting note is that the `triple-rhodf` predicate is itself defined by a set of Datalog clauses! See the GeoPub source code (in the GeoPub.Database.Datalog module).

# We can now use this activity predicate to query for all activities:
activity(?s)?|datalog};
            example_li "Hospitality offers"
              {datalog|# We can query for all ValueFlow proposals that offer hospitality by querying for all proposals that have an intent with resource classified as accomodation:
hospex_proposal(?proposal) :-
    triple(?proposal,type,<https://w3id.org/valueflows#Proposal>),
    triple(?proposal, <https://w3id.org/valueflows#intent>, ?intent),
    triple(?intent, <https://w3id.org/valueflows#resourceClassifiedAs>,<http://w3id.org/hospex/ns#Accommodation>).

hospex_proposal(?p)?
|datalog};
            example_li "Anything that contains the word \"Hello\""
              "triple-fts(?s,?p,?o, \"Hello\")?";
            example_li
              "For resources in the lower Engadin valley (geo-spatial search)"
              "geo(GeoHash(46.7965,10.2965,4), ?s)?";
          ];
      ])

module Parser = struct
  let comment = Angstrom.(char '#' *> many_till any_char (char '\n') >>| ignore)
  let space = Angstrom.(many @@ char ' ' >>| ignore)

  let whitespace =
    Angstrom.(
      many @@ choice [ char ' ' >>| ignore; char '\n' >>| ignore; comment ]
      >>| ignore)

  let query = Angstrom.(Datalog.Atom.parser <* space <* char '?')
  let clauses = Angstrom.(whitespace *> sep_by whitespace Datalog.Clause.parser)

  let program_query =
    Angstrom.(
      (fun clauses query -> (clauses, query))
      <$> whitespace *> sep_by whitespace Datalog.Clause.parser
      <* whitespace <*> query <* whitespace)
end

let parse query_string =
  Angstrom.parse_string ~consume:Angstrom.Consume.Prefix Parser.program_query
    query_string

let view inspector database query_string =
  let query_string_s, query_string_form = query_string_form query_string in

  let query_s = S.map (fun q -> parse q) query_string_s in

  let* results_s =
    S.bind_s query_s (function
      | Ok (clauses, query) ->
          Database.query database ~clauses query
          >|= S.map (fun (_tx, tuples) -> Ok tuples)
      | Error msg -> return @@ S.const (Error msg))
  in

  let inspect = Inspector.show inspector in

  S.l2_s
    (fun results query ->
      match (results, query) with
      | Ok results, Ok (_, query) -> view_results inspect database query results
      | Error msg, _ ->
          return
          @@ El.(
               div ~at:[ UIKit.Alert.danger ]
                 [ txt' @@ "Error while parsing query: " ^ msg ])
      | _, Error msg ->
          return @@ El.(div ~at:[ UIKit.Alert.danger ] [ txt' msg ]))
    results_s query_s
  >|= S.map (fun result_table ->
          El.
            [
              div
                ~at:[ UIKit.container; UIKit.margin ]
                [
                  h1 [ txt' "Datalog" ];
                  help;
                  h2 [ txt' "Query" ];
                  p
                    [
                      txt'
                        "Try composing your own query below. You can use the \
                         pre-defined predicates: triple/3, triple-rhodf/3, \
                         triple-fts/4, fts/2 or geo/2. Variables are prefixed \
                         with a question mark (\"?\"), IRIs are delimited by \
                         angle brackets and strings (for full-text search) \
                         with quotation marks. Geo-spatial queries have the \
                         form ";
                      code [ txt' "GeoHash(lat,long,precision)" ];
                      txt' " where ";
                      code [ txt' "precision" ];
                      txt' " is the precision in number of GeoHash digits.";
                    ];
                  p
                    [
                      txt'
                        "Note that the current Datalog implementation has some \
                         limitations, bugs and is in general slow. It is not \
                         representative of what is possibe with Datalog. \
                         Still, you can have some fun with it!";
                    ];
                  query_string_form;
                  h2 [ txt' "Results" ];
                  result_table;
                ];
            ])
