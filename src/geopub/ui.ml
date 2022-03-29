(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt

let on_el ?(capture = false) ?propagate ?default type' f el =
  let instruct ?(propagate = true) ?(default = true) e =
    if default then () else Ev.prevent_default e;
    if propagate then () else Ev.stop_propagation e
  in
  let opts =
    match capture with
    | false -> None
    | true -> Some (Ev.listen_opts ~capture ())
  in
  let f ev =
    instruct ?propagate ?default ev;
    f ev
  in
  Ev.listen ?opts type' f (El.as_target el);
  el

let geopub_menu (_model : Model.t) =
  let menu_header =
    El.(
      header
        [
          a
            ~at:At.[ href @@ Jstr.v "#about" ]
            [ img ~at:At.[ src (Jstr.v "sgraffito.svg") ] () ];
        ])
  in
  let entry name route =
    El.(li [ a ~at:At.[ href @@ Route.to_jstr route ] [ txt' name ] ])
  in
  El.(
    nav
      ~at:At.[ id @@ Jstr.v "menu" ]
      [
        menu_header;
        nav [ ul [ entry "Map" Route.Map ] ];
        div ~at:At.[ class' @@ Jstr.v "spacer" ] [];
        nav [ ul [ entry "Settings" Route.About ] ];
      ])

let placeholder (model : Model.t) =
  return
  @@ El.
       [
         geopub_menu model;
         div
           ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "text-content" ]
           [ txt' "TODO" ];
       ]

let loading =
  El.
    [
      div
        ~at:At.[ class' @@ Jstr.v "loading" ]
        [
          img ~at:At.[ src (Jstr.v "sgraffito.svg") ] ();
          p [ txt' "Loading GeoPub ..." ];
        ];
    ]

let about =
  El.(
    div
      ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "text-content" ]
      [
        h1 [ txt' "GeoPub" ];
        p [ txt' "Version 0.5.0-dev" ];
        p [ txt' "GeoPub is an experimental XMPP client for geospatial data." ];
        p
          [
            txt' "For more information see ";
            a
              ~at:At.[ href @@ Jstr.v "https://openengiadina.net/" ]
              [ txt' "openengiadina.net" ];
            txt' ".";
          ];
        h2 [ txt' "License" ];
        p
          [
            txt'
              "GeoPub is free software and is licensed under the \
               AGPL-3.0-or-later.";
          ];
        p
          [
            txt'
              "The source code and complete license text is available in the ";
            a
              ~at:
                At.
                  [ href @@ Jstr.v "https://codeberg.org/openEngiadina/geopub" ]
              [ txt' "project repository" ];
            txt' ".";
          ];
      ])
