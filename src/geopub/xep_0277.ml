(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* Support for XEP-0277: Microblogging over XMPP. *)

open Lwt
open Brr
open Reactor_brr

let ns_pubsub_event local = ("http://jabber.org/protocol/pubsub#event", local)

module Post = struct
  type t = { message : Xmppl.Stanza.Message.t; atom : Atom.Entry.t }

  let to_latlng post = Option.map Geoloc.to_latlng post.atom.geoloc
  let to_marker post = Option.map Leaflet.Marker.create @@ to_latlng post
end

let parse (message : Xmppl.Stanza.Message.t) =
  let atom_parser =
    Xmlc.Parser.(
      element (ns_pubsub_event "event") (fun _ ->
          element (ns_pubsub_event "items") (fun _ ->
              element (ns_pubsub_event "item") (fun _ -> Atom.Entry.parser))))
  in
  Lwt_result.catch
    (Xmlc.Tree.parse_trees atom_parser (List.to_seq message.payload))
  >|= Result.map (fun atom : Post.t -> { message; atom })

let view_post ~send_msg (post : Post.t) =
  El.(
    article
      [
        header
          [
            div
              ~at:At.[ class' @@ Jstr.v "post-from" ]
              (List.filter_map
                 (fun x -> x)
                 [
                   Option.map
                     (fun from -> txt' @@ Xmppl.Jid.to_string from)
                     post.message.from;
                 ]);
            div
              ~at:At.[ class' @@ Jstr.v "post-title" ]
              [ txt' post.atom.title ];
            div
              ~at:At.[ class' @@ Jstr.v "post-date" ]
              [ txt' @@ Ptime.to_rfc3339 post.atom.updated ];
          ];
        p ~at:At.[ class' @@ Jstr.v "post-content" ] [ txt' post.atom.content ];
        footer
          [
            div
              ~at:At.[ class' @@ Jstr.v "post-geoloc" ]
              (List.filter_map
                 (fun x -> x)
                 [
                   Option.map
                     (fun (geoloc : Geoloc.t) ->
                       Evr.on_el Ev.click (fun _ ->
                           send_msg @@ `ViewOnMap geoloc)
                       @@ a
                            ~at:At.[ href @@ Jstr.v "#" ]
                            [
                              txt'
                                ((Float.to_string @@ geoloc.latitude)
                                ^ ", " ^ Float.to_string @@ geoloc.longitude);
                            ])
                     post.atom.geoloc;
                 ]);
          ];
      ])
