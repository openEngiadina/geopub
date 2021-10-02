(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

let atom_uri = "http://www.w3.org/2005/Atom"

let ns local = (atom_uri, local)

module Author = struct
  type t = { name : string; uri : string option }

  let make ?uri name = { name; uri }

  let to_xml author =
    Xmlc.(
      make_element (ns "author")
        ~children:
          (List.filter_map
             (fun x -> x)
             [
               Option.some
               @@ make_element (ns "name") ~children:[ make_text author.name ];
               Option.map
                 (fun uri ->
                   make_element (ns "uri") ~children:[ make_text uri ])
                 author.uri;
             ]))

  let parser =
    let required_elements = [ ns "name" ] in
    Xmlc.Parser.(
      element (ns "author") (fun _attributes ->
          ( many
          @@ any_element (fun element_name _attributes ->
                 if element_name = ns "name" then
                   text >>| List.hd >>| fun name ->
                   (element_name, fun author -> { author with name })
                 else if element_name = ns "uri" then
                   text >>| List.hd >>| fun uri ->
                   (element_name, fun author -> { author with uri = Some uri })
                 else return (element_name, fun author -> author))
          >>= fun fields ->
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
            { name = ""; uri = None } fields))
end

module Entry = struct
  type t = {
    id : string;
    title : string;
    content : string;
    authors : Author.t list;
  }

  let make ~title ~content ~authors ~id () = { id; title; content; authors }

  let to_xml entry =
    Xmlc.(
      make_element
        ~attributes:[ (xmlns "xmlns", atom_uri) ]
        ~children:
          ([
             make_element ~children:[ make_text entry.id ] (ns "id");
             make_element ~children:[ make_text entry.title ] (ns "title");
             make_element
               ~attributes:[ (("", "type"), "text") ]
               ~children:[ make_text entry.content ]
               (ns "content");
           ]
          @ List.map Author.to_xml entry.authors)
        (ns "entry"))

  let parser =
    let id_parser =
      Xmlc.Parser.(
        element (ns "id") (fun _attributes ->
            text >>| List.hd >>| fun id ->
            (ns "id", fun entry -> { entry with id })))
    in
    let title_parser =
      Xmlc.Parser.(
        element (ns "title") (fun _attributes ->
            text >>| List.hd >>| fun title ->
            (ns "title", fun entry -> { entry with title })))
    in
    let content_parser =
      Xmlc.Parser.(
        element (ns "content") (fun _attributes ->
            text >>| List.hd >>| fun content ->
            (ns "content", fun entry -> { entry with content })))
    in
    let required_elements = [ ns "id"; ns "title"; ns "content" ] in
    Xmlc.Parser.(
      (* The order of elements in an Atom entry is not specified -
         parsing requires some tricks. *)
      element (ns "entry") (fun _attributes ->
          ( many
          (* Parse child elements of entry into a list *)
          @@ choice
               [
                 id_parser;
                 title_parser;
                 content_parser;
                 ( Author.parser >>= fun author ->
                   return
                     ( ns "author",
                       fun entry ->
                         { entry with authors = author :: entry.authors } ) );
                 (signals >>= fun _ -> return (("", ""), fun x -> x));
               ]
          >>= fun fields ->
            (* Check that all required fields are in the list *)
            if
              List.for_all
                (fun req_element_name -> List.mem_assoc req_element_name fields)
                required_elements
            then return fields
            else fail_with "Atom entry is missing a required element." )
          >>| fun fields ->
          (* Build entry by applying all the parsed field functions *)
          List.fold_left
            (fun entry (_name, field_f) -> field_f entry)
            { id = ""; title = ""; content = ""; authors = [] }
            fields))
end
