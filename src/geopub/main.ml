(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)
open Lwt
open Lwt.Syntax
open Brr
open Brr_react
open Lwt_react
(* open Js_of_ocaml_lwt *)

let src = Logs.Src.create "GeoPub"

module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

(* Model *)

type route = About | Map | Messages

type route_msg = [ `SetRoute of route ]

type msg = route_msg

type model = { route : route; map : Geopub_map.t }
(* { route : route; map : Geopub_map.t; xmpp : Geopub_xmpp.t } *)

let init () = return { route = About; map = Geopub_map.create () }
(* let* xmpp = Geopub_xmpp.init () in
 * return { route = About; map = Geopub_map.create (); xmpp } *)

(* View *)

module Topbar = struct end

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

let topbar _model_s =
  let make_entry name route =
    let a_el = El.(li [ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' name ] ]) in
    (a_el, Evr.on_el Ev.click (function _ev -> `SetRoute route) a_el)
  in
  let entries =
    [
      make_entry "Map" Map;
      make_entry "Messages" Messages;
      make_entry "About" About;
    ]
  in
  let e = entries |> List.map snd |> E.select in
  ( e,
    El.(
      div
        ~at:At.[ id (Jstr.v "topbar") ]
        [
          header [ img ~at:At.[ src (Jstr.v "sgraffito.svg") ] () ];
          nav [ ul List.(map fst entries) ];
        ]) )

let main model_s =
  let main_el = El.div ~at:At.[ id @@ Jstr.v "main" ] [] in
  let msg_e, _ = E.create () in
  let def_signal =
    Elr.def_children main_el
      (S.map
         (fun model ->
           match model.route with
           | Map -> [ Leaflet.get_container model.map ]
           | Messages -> El.[ txt' "Messages" ]
           | About -> [ about_view ])
         model_s)
  in
  (def_signal, msg_e, main_el)

let update msg model = match msg with `SetRoute r -> { model with route = r }

let ui init_model =
  let def model_s =
    let main_def_s, main_msg, main = main model_s in
    let topbar_msg, topbar = topbar model_s in

    (* Keep reference to all update signals *)
    S.keep @@ S.merge (fun _ _ -> ()) () [ main_def_s ];

    (* Collect all events *)
    let msg = E.select [ main_msg; topbar_msg ] in

    (* Perform the model update *)
    let updates = E.map update msg in
    let model_s = S.accum updates (S.value model_s) in
    (model_s, [ topbar; main ])
  in
  (* I admit, I do not understand what this does... TODO wrap my head around S.fix *)
  S.fix init_model def

(* Start the application *)

let setup_logging () =
  Logs.set_reporter @@ Logs_browser.console_reporter ();
  Logs.set_level @@ Some Logs.Info

let main =
  let* init_model = init () in
  return @@ El.set_children (Document.body G.document) (ui init_model)

let () =
  setup_logging ();
  ignore main
