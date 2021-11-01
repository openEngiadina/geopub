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
    Xmlc.Parser.(
      complex ~required:[ ns "name" ] ~ignore_other:true (ns "author")
        (fun _attributes -> return { name = ""; uri = None })
        [
          (* instead of defining multiple parser, we just define one
             `any_element` parser. Just an experiment to try different
             styles (and this is probably more efficient). *)
          any_element (fun element_name _attributes ->
              if element_name = ns "name" then
                text >>| List.hd >>| fun name ->
                (element_name, fun author -> { author with name })
              else if element_name = ns "uri" then
                text >>| List.hd >>| fun uri ->
                (element_name, fun author -> { author with uri = Some uri })
              else return (element_name, fun author -> author));
        ])
end

module Entry = struct
  type t = {
    id : string;
    updated : Ptime.t;
    title : string;
    content : string;
    authors : Author.t list;
  }

  let make ~title ~content ~authors ~id ~updated () =
    { id; updated; title; content; authors }

  let to_xml entry =
    Xmlc.(
      make_element
        ~attributes:[ (xmlns "xmlns", atom_uri) ]
        ~children:
          ([
             make_element ~children:[ make_text entry.id ] (ns "id");
             make_element
               ~children:[ make_text @@ Ptime.to_rfc3339 entry.updated ]
               (ns "updated");
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
    let updated_parser =
      Xmlc.Parser.(
        element (ns "updated") (fun _attributes ->
            text >>| List.hd >>= fun updated_s ->
            match Ptime.(of_rfc3339 updated_s |> rfc3339_error_to_msg) with
            | Ok (updated, _, _) ->
                return (ns "updated", fun entry -> { entry with updated })
            | Error (`Msg msg) -> fail_with msg))
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
    let author_parser =
      Xmlc.Parser.(
        Author.parser >>= fun author ->
        return
          ( ns "author",
            fun entry -> { entry with authors = author :: entry.authors } ))
    in
    Xmlc.Parser.(
      complex
        ~required:[ ns "id"; ns "updated"; ns "title"; ns "content" ]
        ~ignore_other:true (ns "entry")
        (fun _attributes ->
          return
            {
              id = "";
              updated = Ptime.epoch;
              title = "";
              content = "";
              authors = [];
            })
        [
          id_parser; updated_parser; title_parser; content_parser; author_parser;
        ])
end
