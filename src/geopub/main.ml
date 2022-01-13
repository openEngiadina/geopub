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

type model = {
  route : Route.t;
  map : Mapg.t;
  posts : Xep_0277.Post.t list;
  xmpp : Xmpp.model Loadable.t;
}

(* Messages *)

type msg =
  [ (* Navigation *)
    `SetRoute of Route.t
  | (* Hack to fix map *)
    `InvalidateMapSize
  | (* Authentication *)
    `Login of Xmpp.Jid.t * string
  | `LoginDev
  | `LoginDemo
  | `Logout
  | (* XMPP *)
    `XmppLoginResult of (Xmpp.model, exn) Result.t
  | `XmppStateUpdate of Xmpp.Client.state
  | `XmppStanza of Xmpp.Stanza.t
  | (* Database *)
    `DatabaseAdd of (Xep_0277.Post.t, exn) Result.t
  | (* Map      *)
    `ViewOnMap of Xep_0277.Post.t
  | (* Sub-components *)
    `MapMsg of Mapg.msg
  | (* A hack and code smell *)
    `NoOp ]

let init () : (model, msg) Return.t =
  { route = Route.About; map = None; posts = []; xmpp = Loadable.Idle }
  |> Return.singleton
  (* Initialize map *)
  |> Return.command (return @@ `MapMsg Mapg.Init)
  (* dev login *)
  |> Return.command (return `LoginDev)

let update ~stop model msg =
  ignore stop;
  match msg with
  | `SetRoute r -> Return.singleton { model with route = r }
  | `InvalidateMapSize ->
      ignore @@ Option.map Leaflet.Map.invalidate_size model.map;
      model |> Return.singleton
  (* Authentication *)
  | `LoginDev ->
      { model with xmpp = Loadable.Loading }
      |> Return.singleton
      |> Return.command @@ Xmpp.login_dev ()
  | `LoginDemo ->
      { model with xmpp = Loadable.Loading }
      |> Return.singleton
      |> Return.command @@ Xmpp.login_anonymous_demo ()
  | `Login (jid, password) ->
      { model with xmpp = Loadable.Loading }
      |> Return.singleton
      |> Return.command @@ Xmpp.login jid password
  | `Logout -> init ()
  (* XMPP *)
  | `XmppLoginResult (Ok xmpp) ->
      { model with xmpp = Loadable.Loaded xmpp; route = Route.Activity }
      |> Return.singleton
      (* Manually cause a XmppStateUpdate *)
      |> Return.command
           (return
           @@ `XmppStateUpdate (S.value @@ Xmpp.Client.state xmpp.client))
  | `XmppLoginResult (Error _) ->
      { model with route = Route.Login; xmpp = Loadable.Idle }
      |> Return.singleton
  | `XmppStateUpdate state ->
      {
        model with
        xmpp =
          Loadable.map (fun xmpp : Xmpp.model -> { xmpp with state }) model.xmpp;
      }
      |> Return.singleton
  | `XmppStanza (Xmpp.Stanza.Message message) ->
      model |> Return.singleton
      |> Return.command
           (Xep_0277.parse message >|= fun post -> `DatabaseAdd post)
  (* Database *)
  | `DatabaseAdd (Ok post) ->
      { model with posts = post :: model.posts } |> Return.singleton
  | `ViewOnMap _post -> { model with route = Route.Map } |> Return.singleton
  (* Components *)
  | `MapMsg msg ->
      Mapg.update ~send_msg:(fun _ -> ()) model.map msg
      |> Return.map (fun map -> { model with map })
  (* | `ReceiveMessage msg ->
   *     Posts.update
   *       ~send_msg:(fun msg ->
   *         let step = None in
   *         send_msg ?step msg)
   *       model.map model.posts (Posts.ReceiveMessage msg)
   *     |> Return.map (fun posts -> { model with posts }) *)
  | _ -> Return.singleton model

let subscriptions model =
  E.select
    [
      (match model.xmpp with
      | Loadable.Loaded xmpp -> Xmpp.subscriptions xmpp
      | _ -> E.never);
    ]

(* View *)

let about_view =
  El.(
    div
      ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "text-content" ]
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

let menu send_msg (model : model) =
  let on_click msg el = Evr.on_el Ev.click (fun _ -> send_msg msg) el in

  let menu_header jid =
    El.(
      header
        [
          on_click (`SetRoute Route.About)
          @@ a
               ~at:At.[ href @@ Jstr.v "#" ]
               [ img ~at:At.[ src (Jstr.v "sgraffito.svg") ] () ];
          txt' @@ Option.value ~default:"" @@ Option.map Xmpp.Jid.to_string jid;
        ])
  in

  let make_nav_entry name route =
    on_click (`SetRoute route)
      El.(li [ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' name ] ])
  in
  match model.xmpp with
  | Loadable.Loaded { state = Xmpp.Client.Connected jid; _ } ->
      El.(
        nav
          ~at:At.[ id @@ Jstr.v "menu" ]
          [
            menu_header (Some jid);
            nav
              [
                ul
                  [
                    make_nav_entry "Activity" Route.Activity;
                    make_nav_entry "Map" Route.Map;
                  ];
              ];
            div ~at:At.[ class' @@ Jstr.v "spacer" ] [];
            nav
              [
                ul
                  [
                    make_nav_entry "Subscriptions" Route.Activity;
                    on_click `Logout
                    @@ li [ a ~at:At.[ href @@ Jstr.v "#" ] [ txt' "Logout" ] ];
                  ];
              ];
          ])
  | _ ->
      El.(
        nav
          ~at:At.[ id @@ Jstr.v "menu" ]
          [
            menu_header None; nav [ ul [ make_nav_entry "Login" Route.Login ] ];
          ])

let view send_msg model =
  let* content =
    match model.route with
    | Route.Map -> Mapg.view send_msg model.map
    (* | Chat jid -> Chat.view send_msg model.xmpp jid *)
    (* | Route.Posts latlng -> Posts.view send_msg latlng model.xmpp model.posts *)
    (* | Roster jid -> Roster.view send_msg jid model.xmpp *)
    (* | AddContact -> Roster.view_add_contact send_msg model.xmpp *)
    | Route.Activity -> return @@ Activity.view ~send_msg model.posts
    | Route.About -> return [ about_view ]
    | Route.Login -> return [ Login.view send_msg model.xmpp ]
    | _ -> return []
  in
  return (menu send_msg model :: content)

(* A small hack to invalidate the size of the Leaflet map when it is
   dynamically loaded. If not it would not be displayed correctly until a
   manual window resize. *)
let observe_for_map el send_msg =
  let observer records _obs =
    let on_node node =
      match Jv.(to_option to_string @@ get node "id") with
      | Some "map" -> send_msg `InvalidateMapSize
      | _ -> ()
    in

    records
    |> Jv.to_list (fun x -> x)
    |> List.map (fun record ->
           Jv.to_list (fun x -> x) @@ Jv.get record "addedNodes")
    |> List.flatten |> List.iter on_node
  in
  let mutation_observer = Jv.get Jv.global "MutationObserver" in
  let observer = Jv.new' mutation_observer [| Jv.repr observer |] in
  let opts = Jv.obj [| ("childList", Jv.true'); ("subtree", Jv.false') |] in
  ignore @@ Jv.call observer "observe" [| El.to_jv el; opts |]

(* Start the application *)

let main =
  (* Setup logging *)
  Logs.set_reporter @@ Logs_browser.console_reporter ();
  Logs.set_level @@ Some Logs.Info;

  (* Initialize the application *)
  let geopub = App.create ~init ~update ~subscriptions in

  let body = Document.body G.document in

  (* Invalidate map size when it is added to the DOM *)
  observe_for_map body (App.send geopub);

  (* Create a signal that carries the UI *)
  let* ui = S.map_s (view @@ App.send geopub) (App.model geopub) in

  (* Bind the UI to the DOM *)
  S.keep @@ S.map (El.set_children body) ui;

  (* Start the application and return the result *)
  App.result geopub

let () =
  Lwt.on_any main
    (fun v -> Console.error [ "Application unexpectedly stopped"; v ])
    (fun exn -> Console.error [ exn ])
