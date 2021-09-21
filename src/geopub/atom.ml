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
    Xmpp.Xml.(
      make_element
        ~attributes:[ (Xmpp.Xml.Ns.xmlns "xmlns", atom_uri) ]
        ~children:
          [ make_element ~children:[ make_text entry.title ] (ns "title") ]
        (ns "entry"))
end
