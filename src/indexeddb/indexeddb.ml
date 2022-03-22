(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: ISC
 *)

module Request = struct
  let result req = Jv.get req "result"

  let to_lwt req =
    let promise, resolver = Lwt.wait () in
    Jv.set req "onsuccess"
    @@ Jv.repr (fun _ev -> Lwt.wakeup_later resolver @@ result req);
    Jv.set req "onerror"
    @@ Jv.repr (fun _ ->
           let error = Jv.get req "error" in
           Brr.Console.error [ error ];
           Lwt.wakeup_later_exn resolver @@ Jv.Error (Jv.to_error error));
    promise
end

module Database = struct
  type t = Jv.t

  module VersionChange = struct
    type t = Jv.t
    type object_store = Jv.t

    let create_object_store db ?(options = Jv.null) name =
      Jv.call db "createObjectStore" [| Jv.of_jstr name; options |]

    let create_index object_store ~key_path ?(object_parameters = Jv.null) name
        =
      ignore
      @@ Jv.call object_store "createIndex"
           [|
             Jv.of_jstr name;
             Jv.of_list Jv.of_string key_path;
             object_parameters;
           |]
  end

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

  let delete name =
    let request = Jv.call indexeddb "deleteDatabase" [| Jv.of_jstr name |] in
    Request.to_lwt request |> Lwt.map ignore

  let object_store_names db = Jv.get db "objectStoreNames" |> Jv.to_jstr_list
  let close db = ignore @@ Jv.call db "close" [||]
end

module Index = struct
  type t = Jv.t

  let key_path index = Jv.get index "keyPath"

  let count index key =
    Jv.call index "count" [| key |] |> Request.to_lwt |> Lwt.map Jv.to_int

  let get index key = Jv.call index "get" [| key |] |> Request.to_lwt

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
    Jv.call object_store "get" [| key |] |> Request.to_lwt

  let index object_store index_name =
    Jv.call object_store "index" [| Jv.of_jstr index_name |]
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
