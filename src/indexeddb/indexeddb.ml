(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: ISC
 *)

open Lwt

module Request = struct
  let result req = Jv.get req "result"

  let to_lwt req =
    let promise, resolver = Lwt.wait () in
    Jv.set req "onsuccess"
    @@ Jv.repr (fun _ev ->
           Jv.set req "onsuccess" Jv.undefined;
           Lwt.wakeup_later resolver @@ result req);
    Jv.set req "onerror"
    @@ Jv.repr (fun _ ->
           Jv.set req "onerror" Jv.undefined;
           let error = Jv.get req "error" in
           Brr.Console.error [ error ];
           Lwt.wakeup_later_exn resolver @@ Jv.Error (Jv.to_error error));
    promise
end

module Cursor = struct
  type t = Jv.t

  let value t = Jv.get t "value"
  let key t = Jv.get t "key"
  let primary_key t = Jv.get t "primaryKey"

  (* let to_stream cursor =
   *   let stream, push, set_reference = Lwt_stream.create_with_reference () in
   *   Jv.set cursor "onsuccess"
   *   @@ Jv.repr (fun ev ->
   *          match Jv.find_path ev [ "target"; "result" ] with
   *          | Some cursor ->
   *              push @@ Option.some @@ Jv.get cursor "value";
   *              ignore @@ Jv.call cursor "continue" [||]
   *          | None -> push None);
   *   set_reference cursor;
   *   stream *)

  let request t = Jv.get t "request"

  let rec to_seq cursor () =
    let req = request cursor in
    let promise, resolver = Lwt.wait () in
    Jv.set req "onsuccess"
    @@ Jv.repr (fun ev ->
           match Jv.find_path ev [ "target"; "result" ] with
           | Some cursor ->
               Jv.set req "onsuccess" Jv.undefined;
               Lwt.wakeup resolver @@ Lwt_seq.Cons (cursor, to_seq cursor)
           | None -> Lwt.wakeup resolver Lwt_seq.Nil);
    ignore @@ Jv.call cursor "continue" [||];
    promise
end

module Index = struct
  type t = Jv.t

  let key_path index = Jv.get index "keyPath"

  let count index key =
    Jv.call index "count" [| key |] |> Request.to_lwt |> Lwt.map Jv.to_int

  let get index key =
    Jv.call index "get" [| key |]
    |> Request.to_lwt
    >|= Jv.to_option (fun x -> x)

  let get_all index ?count query =
    match count with
    | None ->
        Jv.call index "getAll" [| query |]
        |> Request.to_lwt
        |> Lwt.map (Jv.to_list (fun x -> x))
    | Some count ->
        Jv.call index "getAll" [| query; Jv.of_int count |]
        |> Request.to_lwt
        |> Lwt.map (Jv.to_list (fun x -> x))

  let get_key index query = Jv.call index "getKey" [| query |] |> Request.to_lwt

  let open_cursor index query =
    Jv.call index "openCursor" [| query |]
    |> Request.to_lwt
    >|= Jv.to_option (fun x -> x)
end

module ObjectStore = struct
  type t = Jv.t

  let add object_store ?key value =
    match key with
    | Some key -> Jv.call object_store "add" [| value; key |] |> Request.to_lwt
    | None -> Jv.call object_store "add" [| value |] |> Request.to_lwt

  let put object_store ?key value =
    match key with
    | Some key -> Jv.call object_store "put" [| value; key |] |> Request.to_lwt
    | None -> Jv.call object_store "put" [| value |] |> Request.to_lwt

  let get object_store key =
    Jv.call object_store "get" [| key |]
    |> Request.to_lwt
    >|= Jv.to_option (fun x -> x)

  let get_all object_store ?count query =
    match count with
    | None ->
        Jv.call object_store "getAll" [| query |]
        |> Request.to_lwt
        |> Lwt.map (Jv.to_list (fun x -> x))
    | Some count ->
        Jv.call object_store "getAll" [| query; Jv.of_int count |]
        |> Request.to_lwt
        |> Lwt.map (Jv.to_list (fun x -> x))

  let open_cursor object_store query =
    Jv.call object_store "openCursor" [| query |]
    |> Request.to_lwt
    >|= Jv.to_option (fun x -> x)

  let count index key =
    Jv.call index "count" [| key |] |> Request.to_lwt |> Lwt.map Jv.to_int

  let index object_store index_name =
    Jv.call object_store "index" [| Jv.of_jstr index_name |]

  let create_index object_store ~key_path ?(object_parameters = Jv.null) name =
    Jv.call object_store "createIndex"
      [| Jv.of_jstr name; key_path; object_parameters |]
end

module Database = struct
  type t = Jv.t

  let indexeddb = Jv.get Jv.global "indexedDB"

  let open' ?version ?(on_version_change = fun _ -> ()) name =
    let request =
      Jv.call indexeddb "open"
        [| Jv.of_jstr name; Jv.of_option ~none:Jv.null Jv.of_int version |]
    in
    Jv.set request "onupgradeneeded"
    @@ Jv.repr (fun _ev ->
           let version_change = Request.result request in
           on_version_change version_change);

    request |> Request.to_lwt

  let create_object_store db ?(options = Jv.null) name =
    Jv.call db "createObjectStore" [| Jv.of_jstr name; options |]

  let delete_object_store db name =
    ignore @@ Jv.call db "deleteObjectStore" [| Jv.of_jstr name |]

  let delete name =
    let request = Jv.call indexeddb "deleteDatabase" [| Jv.of_jstr name |] in
    Request.to_lwt request |> Lwt.map ignore

  let object_store_names db = Jv.get db "objectStoreNames" |> Jv.to_jstr_list
  let close db = ignore @@ Jv.call db "close" [||]
end

module Transaction = struct
  type t = Jv.t
  type mode = ReadOnly | ReadWrite

  let mode_to_string = function
    | ReadOnly -> "readonly"
    | ReadWrite -> "readwrite"

  let create db ?(mode = ReadOnly) ?(options = Jv.null) store_names =
    Jv.call db "transaction"
      [|
        Jv.of_list Jv.of_jstr store_names;
        Jv.of_string @@ mode_to_string mode;
        options;
      |]

  let object_store transaction name =
    Jv.call transaction "objectStore" [| Jv.of_jstr name |]

  let commit transaction = ignore @@ Jv.call transaction "commit" [||]
  let abort transaction = ignore @@ Jv.call transaction "abort" [||]
end
