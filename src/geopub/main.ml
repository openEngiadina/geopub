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

type action = [ `InvalidateMapSize | `Xmpp of Geopub_xmpp.action ]

type model = { route : Route.t; map : Geopub_map.t; xmpp : Geopub_xmpp.t }

let init () =
  return
    { route = About; map = Geopub_map.create (); xmpp = Geopub_xmpp.init () }

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
      make_entry "Map" Route.Map;
      make_entry "Messages" Route.Messages;
      make_entry "About" Route.About;
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

(* A small hack to invalidate the size of the Leaflet map when it is
   dynamically loaded. If not it would not be displayed correctly until a
   manual window resize. *)
let observe_map main =
  let e, send = E.create () in
  let observer records _obs =
    let on_node node =
      match Jv.(to_option to_string @@ get node "id") with
      | Some "map" -> send `InvalidateMapSize
      | _ -> ()
    in
    records
    |> List.map (fun record ->
           if Jv.is_some record then
             Jv.to_list (fun x -> x) @@ Jv.get record "addedNodes"
           else [])
    |> List.flatten |> List.iter on_node
  in
  let mutation_observer = Jv.get Jv.global "MutationObserver" in
  let observer = Jv.new' mutation_observer [| Jv.repr observer |] in
  let opts = Jv.obj [| ("childList", Jv.true') |] in
  ignore @@ Jv.call observer "observe" [| El.to_jv main; opts |];
  e

let main ~xmpp model_s =
  let main_el = El.div ~at:At.[ id @@ Jstr.v "main" ] [] in
  let action_e, _ = E.create () in
  Elr.def_children main_el
    (S.map ~eq:( == )
       (fun model ->
         match model.route with
         | Map -> [ Leaflet.get_container model.map ]
         | Messages -> [ xmpp ]
         | About -> [ about_view ])
       model_s)
  (* Keep reference to the signal *)
  |> S.keep;
  (E.select [ action_e; observe_map main_el ], main_el)

let update model action =
  match action with
  | `SetRoute r -> return { model with route = r }
  | `InvalidateMapSize ->
      Leaflet.invalidate_size model.map;
      return model
  | `Xmpp action ->
      let* xmpp' = Geopub_xmpp.update model.xmpp action in
      return { model with xmpp = xmpp' }
  | _ -> return model

let ui init_model =
  let def model_s =
    (* Init the XMPP component *)
    let xmpp_action, xmpp_el =
      Geopub_xmpp.ui (S.map ~eq:( == ) (fun m -> m.xmpp) model_s)
    in

    (* Start the main UI that switches based on the current route *)
    let main_action, main = main ~xmpp:xmpp_el model_s in
    let topbar_action, topbar = topbar model_s in

    (* Collect all actions *)
    let action =
      E.select
        [ main_action; topbar_action; E.map (fun a -> `Xmpp a) xmpp_action ]
    in

    (* Perform the model update *)
    let model_s' = S.fold_s ~eq:( == ) update (S.value model_s) action in

    (* Return the model signal and the HTML elements *)
    (model_s', (model_s', [ topbar; main ]))
  in
  (* Resolve self-reference of model signal on itself (for updates) *)
  S.fix ~eq:( == ) init_model def

(* Start the application *)

let setup_logging () =
  Logs.set_reporter @@ Logs_browser.console_reporter ();
  Logs.set_level @@ Some Logs.Debug

let main =
  let* init_model = init () in
  let model_s, children = ui init_model in
  model_s |> S.keep;
  return @@ El.set_children (Document.body G.document) children

let () =
  setup_logging ();
  ignore main
