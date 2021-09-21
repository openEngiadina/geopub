(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Reactor
open Reactor_brr
open Lwt
open Lwt.Syntax
open Lwt_react

let src = Logs.Src.create "GeoPub"

module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

(* Model *)

type msg = [ `InvalidateMapSize | `XmppMsg of Geopub_xmpp.msg ]

type model = { route : Route.t; map : Geopub_map.t; xmpp : Geopub_xmpp.t }

let init () =
  Return.map
    (fun xmpp -> { route = About; map = Geopub_map.create (); xmpp })
    (Geopub_xmpp.init () |> Return.map_cmd (Lwt.map (fun msg -> `XmppMsg msg)))

let update ~stop ~send_msg model msg =
  ignore stop;
  match msg with
  | `SetRoute r -> Return.singleton { model with route = r }
  | `InvalidateMapSize ->
      Leaflet.invalidate_size model.map;
      Return.singleton model
  | `XmppMsg msg ->
      Geopub_xmpp.update
        ~send_msg:(fun msg ->
          let step = None in
          send_msg ?step msg)
        model.xmpp msg
      |> Return.map (fun xmpp -> { model with xmpp })
  | _ -> Return.singleton model

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

let topbar send_msg model =
  let on_click msg el = Evr.on_el Ev.click (fun _ -> send_msg msg) el in
  let make_entry name route =
    on_click (`SetRoute route)
      El.(li [ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' name ] ])
  in
  let jid = Geopub_xmpp.jid_opt model.xmpp in
  let entries =
    match jid with
    | None -> [ make_entry "Login" Route.Account ]
    | Some jid ->
        [
          make_entry "Map" Route.Map;
          make_entry "Posts" Route.Posts;
          make_entry "Chat" (Route.Chat None);
          make_entry jid Route.Account;
        ]
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
  let* main =
    match model.route with
    | Map -> return [ Leaflet.get_container model.map ]
    | Chat jid -> Geopub_xmpp.chat_view send_msg model.xmpp jid
    | Posts -> Posts.view send_msg model.xmpp
    | Account -> return @@ Geopub_xmpp.account_view send_msg model.xmpp
    | About -> return [ about_view ]
  in
  return [ topbar send_msg model; El.(div ~at:At.[ id @@ Jstr.v "main" ] main) ]

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
  Logs.set_level @@ Some Logs.Info;

  (* Initialize the application *)
  let geopub = App.create ~init ~update in

  let body = Document.body G.document in

  (* Invalidate map size when it is added to the DOM *)
  observe_for_map body (App.send geopub);

  (* Create a signal that carries the UI *)
  let* ui = S.map_s (view @@ App.send geopub) (App.model geopub) in

  (* Bind the UI to the DOM *)
  S.keep @@ S.map (El.set_children body) ui;

  (* Start the application and return the result *)
  App.start geopub;
  App.result geopub

let () =
  Lwt.on_any main
    (fun v -> Console.error [ "Application unexpectedly stopped"; v ])
    (fun exn -> Console.error [ exn ])
