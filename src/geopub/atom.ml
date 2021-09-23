(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

let atom_uri = "http://www.w3.org/2005/Atom"

let ns local = (atom_uri, local)

module Entry = struct
  type t = { title : string }

  let make title = { title }

  let to_xml entry =
    Xmlc.(
      make_element
        ~attributes:[ (xmlns "xmlns", atom_uri) ]
        ~children:
          [ make_element ~children:[ make_text entry.title ] (ns "title") ]
        (ns "entry"))

  let parser =
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
                   (name, fun _entry -> { title })
                 else
                   (* If field is not understood do not modify the entry *)
                   return (name, fun x -> x))
          >>= fun fields ->
            (* Check that all required fields are in the list *)
            if List.mem_assoc (ns "title") fields then return fields
            else fail_with "Atom entry does not have a title." )
          >>| fun fields ->
          (* Build entry by applying all the parsed field functions *)
          List.fold_left
            (fun entry (_name, field_f) -> field_f entry)
            { title = "" } fields))
end
