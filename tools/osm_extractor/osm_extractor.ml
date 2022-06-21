(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

let osmkey =
  Rdf.Namespace.make_namespace "https://www.openstreetmap.org/wiki/Key:"

let geo = Rdf.Namespace.make_namespace "http://www.opengis.net/ont/geosparql#"
let seq_of_ic ic = Seq.of_dispenser (fun () -> In_channel.input_char ic)

let rec group_in_description triples () =
  match triples () with
  | Seq.Cons (ft, rest) ->
      let first_triple : Rdf.Triple.t = ft in
      let description_subject = first_triple.subject in
      let matching_triples =
        Seq.take_while
          (fun (t : Rdf.Triple.t) ->
            Rdf.Triple.Subject.equal description_subject t.subject)
          rest
      in
      let rest = Seq.drop (Seq.length matching_triples) rest in
      let description =
        Seq.fold_left
          (fun description (triple : Rdf.Triple.t) ->
            Rdf.Description.add triple.predicate triple.object' description)
          Rdf.Description.(
            empty description_subject
            |> add first_triple.predicate first_triple.object')
          matching_triples
      in
      Seq.Cons (description, group_in_description rest)
  | Seq.Nil -> Seq.Nil

let man_made_surveillance description =
  Seq.exists
    (fun obj ->
      Rdf.Triple.Object.(
        equal (of_literal @@ Rdf.Literal.make_string "surveillance") obj))
    (Rdf.Description.objects
       (Rdf.Triple.Predicate.of_iri @@ osmkey "man_made")
       description)

let () =
  seq_of_ic stdin |> Rdf_ntriples.parse |> Seq.memoize |> group_in_description
  |> Seq.filter man_made_surveillance
  |> Seq.concat_map Rdf.Description.to_triples
  |> Seq.iter (fun triple ->
         Out_channel.output_string stdout (Rdf_ntriples.encode triple ^ "\n"))

(* let () =
 *   let output = Xmlm.make_output ~nl:true ~indent:(Some 2) (`Channel stdout) in
 *   let rdf local = ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", local) in
 *   let xmlns local = ("http://www.w3.org/2000/xmlns/", local) in
 *   let prefixes =
 *     [
 *       ("osmkey", osmkey "");
 *       ("geo", geo "");
 *       ("osmway", Rdf.Iri.of_string "https://www.openstreetmap.org/way/");
 *       ("osmmeta", Rdf.Iri.of_string "https://www.openstreetmap.org/meta/");
 *       ("osmrel", Rdf.Iri.of_string "https://www.openstreetmap.org/relation/");
 *       ("osmnode", Rdf.Iri.of_string "https://www.openstreetmap.org/node/");
 *       ("osm2rdf", Rdf.Iri.of_string "https://osm2rdf.cs.uni-freiburg.de/rdf#");
 *       ("osm", Rdf.Iri.of_string "https://www.openstreetmap.org/");
 *       ("ogc", Rdf.Iri.of_string "http://www.opengis.net/rdf#");
 *       ("wd", Rdf.Iri.of_string "http://www.wikidata.org/entity/");
 *     ]
 *   in
 *   let start_rdf_element =
 *     `El_start
 *       ( rdf "RDF",
 *         [ (xmlns "rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]
 *         @ (prefixes
 *           |> List.map (fun (prefix, iri) ->
 *                  (xmlns prefix, Rdf.Iri.to_string iri))) )
 *   in
 * 
 *   Xmlm.output output (`Dtd None);
 *   Xmlm.output output start_rdf_element;
 * 
 *   seq_of_ic stdin |> Rdf_ntriples.parse |> Seq.memoize |> group_in_description
 *   |> Seq.filter man_made_surveillance
 *   |> Seq.iter (fun description ->
 *          Rdf_xml.signals_of_description ~prefixes description
 *          |> Seq.iter (Xmlm.output output));
 * 
 *   Xmlm.output output `El_end *)
