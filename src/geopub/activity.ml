(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* View recent activity as a timeline *)

open Brr

let view_post ~send_msg post = El.li [ Xep_0277.view_post ~send_msg post ]

let view ~send_msg posts =
  El.
    [
      div
        ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
        [
          ul
            ~at:At.[ class' @@ Jstr.v "activity" ]
            (List.map (view_post ~send_msg) posts);
        ];
    ]
