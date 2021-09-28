(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

let atom_uri = "http://www.w3.org/2005/Atom"

let ns local = (atom_uri, local)

module Entry = struct
  type t = { title : string; content : string }

  let make ~title ~content () = { title; content }

  let to_xml entry =
    Xmlc.(
      make_element
        ~attributes:[ (xmlns "xmlns", atom_uri) ]
        ~children:
          [
            make_element ~children:[ make_text entry.title ] (ns "title");
            make_element
              ~attributes:[ (("", "type"), "text") ]
              ~children:[ make_text entry.content ]
              (ns "content");
          ]
        (ns "entry"))

  let parser =
    let required_elements = [ ns "title"; ns "content" ] in
    Xmlc.Parser.(
      (* The order of elements in an Atom entry is not specified -
         parsing requires some tricks. *)
      element (ns "entry") (fun _attributes ->
          ( many
          (* Parse child elements of entry into a list *)
          @@ any_element (fun name _attributes ->
                 if name = ns "title" then
                   text >>| List.hd >>| fun title ->
                   (* Return tuples with the name of the entry and a
                      function that modifies the entry to set the
                      field. *)
                   (name, fun entry -> { entry with title })
                 else if name = ns "content" then
                   text >>| List.hd >>| fun content ->
                   (name, fun entry -> { entry with content })
                 else
                   (* If field is not understood do not modify the entry *)
                   return (name, fun x -> x))
          >>= fun fields ->
            (* Check that all required fields are in the list *)
            if
              List.for_all
                (fun req_element_name -> List.mem_assoc req_element_name fields)
                required_elements
            then return fields
            else fail_with "Atom entry does not have a title." )
          >>| fun fields ->
          (* Build entry by applying all the parsed field functions *)
          List.fold_left
            (fun entry (_name, field_f) -> field_f entry)
            { title = ""; content = "" }
            fields))
end
