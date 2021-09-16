(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Reactor
open Reactor_brr
open Lwt_react

let src = Logs.Src.create "GeoPub"

module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

(* Model *)

type msg = [ `InvalidateMapSize | `XmppMsg of Geopub_xmpp.msg ]

type model = { route : Route.t; map : Geopub_map.t; xmpp : Geopub_xmpp.t }

let init () =
  Return.map
    (fun xmpp -> { route = About; map = Geopub_map.create (); xmpp })
    (Geopub_xmpp.init ())

(* View *)

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

(* let main ~xmpp model_s =
 *   let main_el = El.div ~at:At.[ id @@ Jstr.v "main" ] [] in
 *   let action_e, _ = E.create () in
 *   Elr.def_children main_el
 *     (S.map ~eq:( == )
 *        (fun model ->
 *          match model.route with
 *          | Map -> [ Leaflet.get_container model.map ]
 *          | Messages -> [ xmpp ]
 *          | About -> [ about_view ])
 *        model_s)
 *   (\* Keep reference to the signal *\)
 *   |> S.keep;
 *   (E.select [ action_e; observe_map main_el ], main_el) *)

let update ~stop ~send_msg model msg =
  ignore stop;
  ignore send_msg;
  match msg with
  | `SetRoute r -> Return.singleton { model with route = r }
  | `InvalidateMapSize ->
      Leaflet.invalidate_size model.map;
      Return.singleton model
  | `XmppMsg msg ->
      Geopub_xmpp.update ~send_msg model.xmpp msg
      |> Return.map (fun xmpp -> { model with xmpp })
      |> Return.map_cmd (Lwt.map (fun msg -> `XmppMsg msg))
  | _ -> Return.singleton model

(* let ui init_model =
 *   let def model_s =
 *     (\* Init the XMPP component *\)
 *     let xmpp_action, xmpp_el =
 *       Geopub_xmpp.ui (S.map ~eq:( == ) (fun m -> m.xmpp) model_s)
 *     in
 * 
 *     (\* Start the main UI that switches based on the current route *\)
 *     let main_action, main = main ~xmpp:xmpp_el model_s in
 *     let topbar_action, topbar = topbar model_s in
 * 
 *     (\* Collect all actions *\)
 *     let action =
 *       E.select
 *         [ main_action; topbar_action; E.map (fun a -> `Xmpp a) xmpp_action ]
 *     in
 * 
 *     (\* Perform the model update *\)
 *     let model_s' = S.fold_s ~eq:( == ) update (S.value model_s) action in
 * 
 *     (\* Return the model signal and the HTML elements *\)
 *     (model_s', (model_s', [ topbar; main ]))
 *   in
 *   (\* Resolve self-reference of model signal on itself (for updates) *\)
 *   S.fix ~eq:( == ) init_model def *)

let topbar send_msg _model =
  let on_click msg el =
    Reactor_brr.Evr.on_el Ev.click (fun _ -> send_msg msg) el
  in
  let make_entry name route =
    on_click (`SetRoute route)
      El.(li [ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' name ] ])
  in
  let entries =
    [ make_entry "Map" Route.Map; make_entry "Messages" Route.Messages ]
  in
  El.(
    div
      ~at:At.[ id (Jstr.v "topbar") ]
      [
        header
          [
            on_click (`SetRoute Route.About)
            @@ a
                 ~at:At.[ href @@ Jstr.v "#" ]
                 [ img ~at:At.[ src (Jstr.v "sgraffito.svg") ] () ];
          ];
        nav [ ul entries ];
      ])

let view send_msg model =
  let main =
    El.(
      div
        ~at:At.[ id @@ Jstr.v "main" ]
        (match model.route with
        | Map -> [ Leaflet.get_container model.map ]
        | Messages ->
            Geopub_xmpp.view (fun msg -> send_msg @@ `XmppMsg msg) model.xmpp
        | About -> [ about_view ]))
  in
  [ topbar send_msg model; main ]

(* A small hack to invalidate the size of the Leaflet map when it is
   dynamically loaded. If not it would not be displayed correctly until a
   manual window resize. *)
let observe_for_map el send_msg =
  let observer records _obs =
    let on_node node =
      let el = El.to_jv node in
      match Jv.(to_option to_string @@ get el "id") with
      | Some "map" -> send_msg `InvalidateMapSize
      | _ -> ()
    in
    records
    |> Jv.to_list (fun x -> x)
    |> List.map (fun record ->
           Jv.to_list (fun node -> El.children @@ El.of_jv node)
           @@ Jv.get record "addedNodes")
    |> List.flatten |> List.flatten |> List.iter on_node
  in
  let mutation_observer = Jv.get Jv.global "MutationObserver" in
  let observer = Jv.new' mutation_observer [| Jv.repr observer |] in
  let opts = Jv.obj [| ("childList", Jv.true'); ("subtree", Jv.true') |] in
  ignore @@ Jv.call observer "observe" [| El.to_jv el; opts |]

(* Start the application *)

let main =
  (* Setup logging *)
  Logs.set_reporter @@ Logs_browser.console_reporter ();
  Logs.set_level @@ Some Logs.Debug;

  (* Initialize the application *)
  let geopub = App.create ~init ~update in

  let body = Document.body G.document in

  (* Invalidate map size when it is added to the DOM *)
  observe_for_map body (App.send geopub);

  (* Create a signal that carries the UI *)
  let ui = S.map (view @@ App.send geopub) (App.model geopub) in

  (* Bind the UI to the DOM *)
  S.keep @@ S.map (El.set_children body) ui;

  (* Start the application and return the result *)
  App.start geopub;
  App.result geopub

let () = ignore main
