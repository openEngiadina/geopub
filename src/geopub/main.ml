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

type model = { route : Route.t; map : Mapg.t; posts : Posts.t; xmpp : Xmpp.t }

let init () =
  Return.map
    (fun xmpp -> { route = About; map = None; posts = []; xmpp })
    (Xmpp.init () |> Return.map_cmd (Lwt.map (fun msg -> `XmppMsg msg)))
  |> Return.command (return @@ `MapMsg Mapg.Init)

let update ~stop ~send_msg model msg =
  ignore stop;
  match msg with
  | `SetRoute r -> Return.singleton { model with route = r }
  | `InvalidateMapSize ->
      ignore @@ Option.map Leaflet.Map.invalidate_size model.map;
      Return.singleton model
  | `XmppMsg msg ->
      Xmpp.update
        ~send_msg:(fun msg ->
          let step = None in
          send_msg ?step msg)
        model.xmpp msg
      |> Return.map (fun xmpp -> { model with xmpp })
  | `PostsMsg msg ->
      Posts.update
        ~send_msg:(fun msg ->
          let step = None in
          send_msg ?step msg)
        model.map model.posts msg
      |> Return.map (fun posts -> { model with posts })
  | `MapMsg msg ->
      Mapg.update
        ~send_msg:(fun msg ->
          let step = None in
          send_msg ?step msg)
        model.map msg
      |> Return.map (fun map -> { model with map })
  | `ReceiveMessage msg ->
      Posts.update
        ~send_msg:(fun msg ->
          let step = None in
          send_msg ?step msg)
        model.map model.posts (Posts.ReceiveMessage msg)
      |> Return.map (fun posts -> { model with posts })
  | _ -> Return.singleton model

(* View *)

let about_view =
  El.(
    div
      ~at:At.[ class' @@ Jstr.v "text-content" ]
      [
        h1 [ txt' "GeoPub" ];
        p [ txt' "Version 0.4.0-dev" ];
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

let menu send_msg model =
  let on_click msg el = Evr.on_el Ev.click (fun _ -> send_msg msg) el in

  let jid = Xmpp.jid_opt model.xmpp in
  let menu_header =
    El.(
      header
        [
          on_click (`SetRoute Route.About)
          @@ a
               ~at:At.[ href @@ Jstr.v "#" ]
               [ img ~at:At.[ src (Jstr.v "sgraffito.svg") ] () ];
          txt' @@ Option.value ~default:"" jid;
        ])
  in

  let make_nav_entry name route =
    on_click (`SetRoute route)
      El.(li [ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' name ] ])
  in
  match jid with
  | Some _ ->
      El.(
        nav
          ~at:At.[ id @@ Jstr.v "menu" ]
          [
            menu_header;
            nav
              [
                ul
                  [
                    make_nav_entry "Activity" (Route.Posts None);
                    make_nav_entry "Map" Route.Map;
                  ];
              ];
            div ~at:At.[ class' @@ Jstr.v "spacer" ] [];
            nav
              [
                ul
                  [
                    make_nav_entry "" (Route.Posts None);
                    on_click (`XmppMsg Xmpp.Logout)
                    @@ li [ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' "Logout" ] ];
                  ];
              ];
          ])
  | None ->
      El.(
        nav
          ~at:At.[ id @@ Jstr.v "menu" ]
          [ menu_header; nav [ ul [ make_nav_entry "Login" Route.Login ] ] ])

let view send_msg model =
  let* main =
    match model.route with
    | Map -> Mapg.view send_msg model.map
    (* | Chat jid -> Chat.view send_msg model.xmpp jid *)
    | Posts latlng -> Posts.view send_msg latlng model.xmpp model.posts
    | Roster jid -> Roster.view send_msg jid model.xmpp
    | AddContact -> Roster.view_add_contact send_msg model.xmpp
    | About -> return [ about_view ]
    | Login -> return [ Login.view send_msg model.xmpp ]
  in
  return [ menu send_msg model; El.(div ~at:At.[ id @@ Jstr.v "main" ] main) ]

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
