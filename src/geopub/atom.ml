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
    Xmlc.Tree.(
      make_element (ns "author")
        ~children:
          (List.filter_map
             (fun x -> x)
             [
               Option.some
               @@ make_element (ns "name") ~children:[ make_data author.name ];
               Option.map
                 (fun uri ->
                   make_element (ns "uri") ~children:[ make_data uri ])
                 author.uri;
             ]))

  let parser =
    Xmlc.Parser.(
      complex
        ~required:[ ns "name" ]
        ~ignore_other:true (ns "author")
        (fun _attributes -> return { name = ""; uri = None })
        [
          (* instead of defining multiple parser, we just define one
             `any_element` parser. Just an experiment to try different
             styles (and this is probably more efficient). *)
          any_element (fun (element_name, _attributes) ->
              if element_name = ns "name" then
                data >>| fun name ->
                (element_name, fun author -> { author with name })
              else if element_name = ns "uri" then
                data >>| fun uri ->
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
    (* As described in [XEP-0277: Microblogging over XMPP](https://xmpp.org/extensions/xep-0277.html#geotagging) *)
    geoloc : Geoloc.t option;
  }

  let make ~title ~content ~authors ~id ~updated ?geoloc () =
    { id; updated; title; content; authors; geoloc }

  let to_xml entry =
    Xmlc.Tree.(
      make_element
        ~attributes:[ (Xmlc.Namespace.xmlns "xmlns", atom_uri) ]
        ~children:
          (List.filter_map
             (fun x -> x)
             [
               Option.some
               @@ make_element ~children:[ make_data entry.id ] (ns "id");
               Option.some
               @@ make_element
                    ~children:[ make_data @@ Ptime.to_rfc3339 entry.updated ]
                    (ns "updated");
               Option.some
               @@ make_element ~children:[ make_data entry.title ] (ns "title");
               Option.some
               @@ make_element
                    ~attributes:[ (("", "type"), "text") ]
                    ~children:[ make_data entry.content ]
                    (ns "content");
               Option.map (fun geoloc -> Geoloc.to_xml geoloc) entry.geoloc;
             ]
          @ List.map Author.to_xml entry.authors)
        (ns "entry"))

  let parser =
    let id_parser =
      Xmlc.Parser.(
        element (ns "id") (fun _attributes ->
            data >>| fun id -> (ns "id", fun entry -> { entry with id })))
    in
    let updated_parser =
      Xmlc.Parser.(
        element (ns "updated") (fun _attributes ->
            data >>= fun updated_s ->
            match Ptime.(of_rfc3339 updated_s |> rfc3339_error_to_msg) with
            | Ok (updated, _, _) ->
                return (ns "updated", fun entry -> { entry with updated })
            | Error (`Msg msg) -> fail_with msg))
    in
    let title_parser =
      Xmlc.Parser.(
        element (ns "title") (fun _attributes ->
            data >>| fun title -> (ns "title", fun entry -> { entry with title })))
    in
    let content_parser =
      Xmlc.Parser.(
        element (ns "content") (fun _attributes ->
            data >>| fun content ->
            (ns "content", fun entry -> { entry with content })))
    in
    let author_parser =
      Xmlc.Parser.(
        Author.parser >>= fun author ->
        return
          ( ns "author",
            fun entry -> { entry with authors = author :: entry.authors } ))
    in
    let geoloc_parser =
      Xmlc.Parser.(
        Geoloc.parser >>= fun geoloc ->
        return
          (Geoloc.ns "geoloc", fun entry -> { entry with geoloc = Some geoloc }))
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
              geoloc = None;
            })
        [
          id_parser;
          updated_parser;
          title_parser;
          content_parser;
          author_parser;
          geoloc_parser;
        ])
end
