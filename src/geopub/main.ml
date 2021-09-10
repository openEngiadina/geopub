(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)
open Lwt

(* open Lwt.Syntax *)
open Brr
open Brr_react
open Lwt_react
(* open Js_of_ocaml_lwt *)

let src = Logs.Src.create "GeoPub"

module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

(* Model *)

type route = About | Map | Messages

type model = { route : route; map : Geopub_map.t }

let init () = { route = About; map = Geopub_map.create () }

let model_s, update_model = S.create @@ init ()

let update_route route =
  let model = S.value model_s in
  update_model { model with route }

(* View *)

module Topbar = struct
  let make_entry name route =
    let a_el = El.(a ~at:At.[ href @@ Jstr.v "#" ] [ txt' name ]) in
    Ev.listen Ev.click
      (function _ev -> update_route route)
      (El.as_target a_el);
    El.(li [ a_el ])

  let view () =
    El.(
      div
        ~at:At.[ id (Jstr.v "topbar") ]
        [
          header [ img ~at:At.[ src (Jstr.v "sgraffito.svg") ] () ];
          nav
            [
              ul
                [
                  make_entry "Map" Map;
                  make_entry "Messages" Messages;
                  make_entry "About" About;
                ];
            ];
        ])
end

let about_view =
  El.(
    div
      ~at:At.[ class' @@ Jstr.v "text-content" ]
      [
        h1 [ txt' "GeoPub" ];
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

let view model =
  let main_view = function
    | About -> about_view
    | Messages -> El.txt' "GeoPub is also an XMPP client!"
    | Map -> Leaflet.get_container model.map
  in
  El.
    [
      Topbar.view ();
      div ~at:At.[ id (Jstr.v "main") ] [ main_view model.route ];
    ]

(* Start the application *)

let setup_logging () =
  Logs.set_reporter @@ Logs_browser.console_reporter ();
  Logs.set_level @@ Some Logs.Info

let main =
  let _view_s =
    Elr.def_children (Document.body G.document) S.(map view model_s)
  in

  return_unit

let () =
  setup_logging ();
  ignore main
