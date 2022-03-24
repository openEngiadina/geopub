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
    Indexeddb.Database.open' ~version:geopub_database_version
      ~on_version_change:(fun db ->
        let open Indexeddb.Database.VersionChange in
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
    | "rdf_db", [ None; None; None ] ->
        ObjectStore.get_all triples Jv.undefined |> stream_of_list
    | "rdf_db", [ Some s; None; None ] ->
        let s_index = ObjectStore.index triples (Jstr.v "s") in
        Index.get_all s_index Encoding.(jv_of_terms [ s ]) |> stream_of_list
    | "rdf_db", [ None; Some p; None ] ->
        let p_index = ObjectStore.index triples (Jstr.v "p") in
        Index.get_all p_index Encoding.(jv_of_terms [ p ]) |> stream_of_list
    | "rdf_db", [ None; None; Some o ] ->
        let o_index = ObjectStore.index triples (Jstr.v "o") in
        Index.get_all o_index Encoding.(jv_of_terms [ o ]) |> stream_of_list
    | "rdf_db", [ Some s; Some p; None ] ->
        let sp_index = ObjectStore.index triples (Jstr.v "sp") in
        Index.get_all sp_index Encoding.(jv_of_terms [ s; p ]) |> stream_of_list
    | "rdf_db", [ Some s; None; Some o ] ->
        let so_index = ObjectStore.index triples (Jstr.v "so") in
        Index.get_all so_index Encoding.(jv_of_terms [ s; o ]) |> stream_of_list
    | "rdf_db", [ None; Some p; Some o ] ->
        let po_index = ObjectStore.index triples (Jstr.v "po") in
        Index.get_all po_index Encoding.(jv_of_terms [ p; o ]) |> stream_of_list
    | "rdf_db", [ Some s; Some p; Some o ] ->
        let spo_index = ObjectStore.index triples (Jstr.v "spo") in
        Index.get_all spo_index Encoding.(jv_of_terms [ s; p; o ])
        |> stream_of_list
    | _, _ -> Lwt_stream.of_list []
end

module Datalog = Datalogl.Make (struct
  type t = Rdf.Term.t

  let compare = Rdf.Term.compare
  let parser = Angstrom.fail "Can not parse RDF terms from Datalog queries yet."
  let pp = Rdf.Term.pp
end)

let test_datalog db =
  (* let tx =
   *   Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadOnly
   *     [ Database.triples_object_store_name ]
   * in
   * Database.edb tx "rdf_db" [ None; None; None ]
   * |> Lwt_stream.iter (fun constants ->
   *        Log.debug (fun m -> m "EDB: %a" (Fmt.list Rdf.Term.pp) constants)) *)
  let tx =
    Indexeddb.Transaction.create db ~mode:Indexeddb.Transaction.ReadOnly
      [ Database.triples_object_store_name ]
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

  let* tuples = Datalog.query ~database:(Database.edb tx) ~program query in

  Log.debug (fun m -> m "inspect: %a" Datalog.Tuple.Set.pp tuples);
  return_unit
