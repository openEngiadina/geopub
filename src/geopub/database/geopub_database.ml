(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax

(* Setup logging *)
let src = Logs.Src.create "GeoPub.Database"

module Log = (val Logs.src_log src : Logs.LOG)

module Database = struct
  type t = Indexeddb.Database.t

  (* Constants *)
  let geopub_database_version = 1
  let geopub_database_name = "GeoPub"
  let triples_object_store_name = Jstr.v "triples"

  let init () =
    (* let* () = Indexeddb.Database.delete @@ Jstr.v geopub_database_name in *)
    Log.info (fun m -> m "Initializing GeoPub databse.");

    Indexeddb.Database.open' ~version:geopub_database_version
      ~on_version_change:(fun db ->
        let open Indexeddb.Database.VersionChange in
        Log.debug (fun m -> m "Performing database version change.");

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

  let add_triple tx (triple : Rdf.Triple.t) =
    let triples =
      Indexeddb.Transaction.object_store tx triples_object_store_name
    in
    let spo = Indexeddb.ObjectStore.index triples (Jstr.v "spo") in
    let* count =
      Indexeddb.Index.count spo
        (Encoding.jv_of_terms
           [
             Rdf.Triple.Subject.to_term triple.subject;
             Rdf.Triple.Predicate.to_term triple.predicate;
             Rdf.Triple.Object.to_term triple.object';
           ])
    in
    if count = 0 then
      (* TODO usage of jv_of_triple is inefficient as we have already encoded triples into Buffers above. This should be reused. *)
      let* _ =
        Indexeddb.ObjectStore.add triples (Encoding.jv_of_triple triple)
      in
      return_unit
    else return_unit

  let add_rdf db graph =
    let tx =
      Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadWrite
        [ triples_object_store_name ]
    in
    let* () =
      Rdf.Graph.to_triples graph |> List.of_seq
      |> Lwt_list.iter_p (add_triple tx)
    in
    return @@ Indexeddb.Transaction.commit tx

  let edb tx predicate pattern =
    let stream_of_list l_p =
      (* TODO instead of transforming list to Lwt_stream we should use IDBCursor. *)
      let stream, push, set_reference = Lwt_stream.create_with_reference () in
      let pusher =
        l_p
        >|= List.iter (fun el ->
                let tuple = Encoding.tuple_of_jv_exn el in
                push (Some tuple))
        (* Close stream after all elements are pushed *)
        >|= fun () -> push None
      in
      (* Don't forget the pusher *)
      set_reference pusher;
      stream
    in

    let open Indexeddb in
    (* Open the triples object store *)
    let triples = Transaction.object_store tx triples_object_store_name in
    (* Get triples with index matching the query pattern *)
    match (predicate, pattern) with
    | "triples", [ None; None; None ] ->
        ObjectStore.get_all triples Jv.undefined |> stream_of_list
    | "triples", [ Some s; None; None ] ->
        let s_index = ObjectStore.index triples (Jstr.v "s") in
        Index.get_all s_index Encoding.(jv_of_terms [ s ]) |> stream_of_list
    | "triples", [ None; Some p; None ] ->
        let p_index = ObjectStore.index triples (Jstr.v "p") in
        Index.get_all p_index Encoding.(jv_of_terms [ p ]) |> stream_of_list
    | "triples", [ None; None; Some o ] ->
        let o_index = ObjectStore.index triples (Jstr.v "o") in
        Index.get_all o_index Encoding.(jv_of_terms [ o ]) |> stream_of_list
    | "triples", [ Some s; Some p; None ] ->
        let sp_index = ObjectStore.index triples (Jstr.v "sp") in
        Index.get_all sp_index Encoding.(jv_of_terms [ s; p ]) |> stream_of_list
    | "triples", [ Some s; None; Some o ] ->
        let so_index = ObjectStore.index triples (Jstr.v "so") in
        Index.get_all so_index Encoding.(jv_of_terms [ s; o ]) |> stream_of_list
    | "triples", [ None; Some p; Some o ] ->
        let po_index = ObjectStore.index triples (Jstr.v "po") in
        Index.get_all po_index Encoding.(jv_of_terms [ p; o ]) |> stream_of_list
    | "triples", [ Some s; Some p; Some o ] ->
        let spo_index = ObjectStore.index triples (Jstr.v "spo") in
        Index.get_all spo_index Encoding.(jv_of_terms [ s; p; o ])
        |> stream_of_list
    | _, _ -> Lwt_stream.of_list []
end

module Datalog = Datalogl.Make (struct
  type t = Rdf.Term.t

  let compare = Rdf.Term.compare
  let parser = Encoding.parser
  let pp = Rdf.Term.pp
end)

let datalog_program =
  {datalog|
   rdf(?s,?p,?o) :- triples(?s,?p,?o).
   |datalog}
  |> Angstrom.parse_string ~consume:Angstrom.Consume.All Datalog.Program.parser
  |> Result.get_ok

let query db q =
  let tx =
    Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadOnly
      [ Database.triples_object_store_name ]
  in
  Datalog.query ~database:(Database.edb tx) ~program:datalog_program q

let query_string db q =
  let* q =
    q |> Angstrom.parse_string ~consume:Angstrom.Consume.All Datalog.Atom.parser
    |> function
    | Ok query -> return query
    | Error msg -> Lwt.fail_with msg
  in
  query db q

let get_description db subject =
  let q =
    Datalog.(
      Atom.make "rdf"
        [
          Term.make_constant @@ Rdf.Triple.Subject.to_term subject;
          Term.make_variable "p";
          Term.make_variable "o";
        ])
  in
  query db q >|= Datalog.Tuple.Set.to_seq
  >|= Seq.filter_map (function
        | [ s; p; o ] ->
            Rdf.Triple.(
              Option.some
              @@ make
                   (Rdf.Term.map s Subject.of_iri Subject.of_blank_node
                      (fun _ ->
                        failwith "unexpected literal in subject position"))
                   (Rdf.Term.map p Predicate.of_iri
                      (fun _ ->
                        failwith "unexpected blank node in predicate position")
                      (fun _ ->
                        failwith "unexpected literal in predicate position"))
                   (Object.of_term o))
        | _ -> None)
  >|= Seq.fold_left
        (fun graph triple -> Rdf.Graph.add triple graph)
        Rdf.Graph.empty
  >|= Rdf.Graph.description subject

let test_datalog db =
  let* tuples = query_string db {query|rdf(?s,rdf:type,as:Note)|query} in

  Log.debug (fun m -> m "inspect: %a" Datalog.Tuple.Set.pp tuples);
  return_unit
