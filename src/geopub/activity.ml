(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* View recent activity as a timeline *)

open Brr
open Reactor_brr

let view_post ~send_msg post = El.li [ Xep_0277.view_post ~send_msg post ]

let view ~send_msg posts graph =
  let activities = Activitystreams.activities graph in
  El.
    [
      div
        ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
        [
          h2 [ txt' "Activity" ];
          ul
            ~at:At.[ class' @@ Jstr.v "activity" ]
            (List.map (view_post ~send_msg) posts
            @ List.of_seq
            @@ Seq.map
                 (Activitystreams.view_activity ~send_msg graph)
                 activities);
          Evr.on_el Ev.click (fun _ ->
              send_msg @@ `SetActionBar (Some (Route.NewPost None)))
          @@ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' "New Post" ];
        ];
    ]
