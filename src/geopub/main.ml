(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
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
  action_bar : Route.action option;
  map : Mapg.t;
  posts : Xep_0277.Post.t list;
  graph : Rdf.Graph.t;
  xmpp : Xmpp.model Loadable.t;
  roster : Xmpp.Roster.roster;
}

(* Messages *)

type msg =
  [ (* Navigation *)
    `SetRoute of Route.t
  | `SetActionBar of Route.action option
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
  | `XmppRosterUpdate of Xmpp.Roster.roster
  | `XmppRosterAddContact of Xmpp.Jid.t
  | `XmppStanza of Xmpp.Stanza.t
  | `XmppPresenceDenySubscription of Xmpp.Jid.t
  | `XmppPresenceApproveSubscription of Xmpp.Jid.t
  | `XmppPresenceUnsubscribe of Xmpp.Jid.t
  | `XmppPresenceSubscribe of Xmpp.Jid.t
  | (* ActivityStreams *)
    `PostActivityStreamsNote of Activitystreams.Note.t
  | (* Database *)
    `AddPost of (Xep_0277.Post.t, exn) Result.t
  | `AddRdf of (Rdf.Graph.t, exn) Result.t
  | (* Map      *)
    `ViewOnMap of Geoloc.t
  | (* A hack and code smell *)
    `NoOp ]

let init () : (model, msg) Return.t =
  {
    route = Route.About;
    action_bar = None;
    map = Mapg.init ();
    posts = [];
    graph = Rdf.Graph.empty;
    xmpp = Loadable.Idle;
    roster = Xmpp.Jid.Map.empty;
  }
  |> Return.singleton
  (* dev login *)
  |> Return.command (return `LoginDev)

let command_with_xmpp_conected f =
  Return.bind (fun model ->
      match model.xmpp with
      | Loadable.Loaded ({ state = Xmpp.Client.Connected jid; _ } as xmpp) ->
          model |> Return.singleton |> Return.command (f jid xmpp)
      | _ -> model |> Return.singleton)

let post_activitystreams_note note jid xmpp =
  ignore xmpp;
  let create_id, create = note |> Activitystreams.Note.as_create jid in
  let* as_xml = Activitystreams.rdf_to_xml create in
  Xmpp.publish_activitystreams jid xmpp create_id as_xml >|= fun _ -> `NoOp

let update ~stop model msg =
  ignore stop;
  match msg with
  | `SetRoute r -> Return.singleton { model with route = r }
  | `SetActionBar action_bar -> { model with action_bar } |> Return.singleton
  | `InvalidateMapSize ->
      Mapg.invalidate_size model.map;
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
      |> Return.command (return @@ `XmppRosterUpdate (S.value @@ xmpp.roster))
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
  | `XmppRosterUpdate roster ->
      Console.log [ Jstr.v @@ "RosterUpdate" ];
      { model with roster } |> Return.singleton
  | `XmppRosterAddContact jid ->
      model |> Return.singleton
      |> Return.command
           (model.xmpp |> Loadable.to_option
           |> Option.map (fun xmpp -> Xmpp.roster_add xmpp jid)
           |> Option.value ~default:(return `NoOp))
  | `XmppStanza (Xmpp.Stanza.Message message) ->
      model |> Return.singleton
      (* Attempt to parse as XEP-0277 post *)
      |> Return.command (Xep_0277.parse message >|= fun post -> `AddPost post)
      (* or as RDF *)
      |> Return.command
           (Activitystreams.parse message >|= fun graph -> `AddRdf graph)
  | `XmppPresenceDenySubscription jid ->
      model |> Return.singleton
      |> Return.command
           (model.xmpp |> Loadable.to_option
           |> Option.map (fun xmpp -> Xmpp.deny_presence_subscription xmpp jid)
           |> Option.value ~default:(return `NoOp))
  | `XmppPresenceApproveSubscription jid ->
      model |> Return.singleton
      |> Return.command
           (model.xmpp |> Loadable.to_option
           |> Option.map (fun xmpp ->
                  Xmpp.approve_presence_subscription xmpp jid)
           |> Option.value ~default:(return `NoOp))
  | `XmppPresenceSubscribe jid ->
      model |> Return.singleton
      |> Return.command
           (model.xmpp |> Loadable.to_option
           |> Option.map (fun xmpp -> Xmpp.presence_subscribe xmpp jid)
           |> Option.value ~default:(return `NoOp))
  | `XmppPresenceUnsubscribe jid ->
      model |> Return.singleton
      |> Return.command
           (model.xmpp |> Loadable.to_option
           |> Option.map (fun xmpp -> Xmpp.presence_unsubscribe xmpp jid)
           |> Option.value ~default:(return `NoOp))
  | `PostActivityStreamsNote note ->
      model |> Return.singleton
      |> command_with_xmpp_conected (post_activitystreams_note note)
      |> Return.command (return @@ `SetActionBar None)
  (* Database *)
  | `AddPost (Ok post) ->
      {
        model with
        map = Mapg.add_post post model.map;
        posts = post :: model.posts;
      }
      |> Return.singleton
  | `AddRdf (Ok rdf) ->
      Format.printf "%a" Rdf.Graph.pp rdf;
      { model with graph = Rdf.Graph.union model.graph rdf } |> Return.singleton
  | `ViewOnMap geoloc ->
      {
        model with
        route = Route.Map;
        map = Mapg.set_view (Geoloc.to_latlng geoloc) model.map;
      }
      |> Return.singleton
  (* Catch all *)
  | _ -> Return.singleton model

let subscriptions model =
  E.select
    [
      (match model.xmpp with
      | Loadable.Loaded xmpp -> Xmpp.subscriptions xmpp
      | _ -> E.never);
      (* TODO handle create post here *)
      Mapg.subscriptions model.map
      |> E.map (function
           | Mapg.CreatePost latlng ->
               `SetActionBar (Option.some @@ Route.NewPost (Some latlng))
           | _ -> `NoOp);
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
          txt' @@ Option.value ~default:""
          @@ Option.map
               (fun jid -> jid |> Xmpp.Jid.bare |> Xmpp.Jid.to_string)
               jid;
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
                    make_nav_entry "Contacts" Route.Roster;
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
  let* action_bar =
    match model.action_bar with
    | Some (Route.NewPost latlng) ->
        return @@ [ Activitystreams.view_compose ?latlng ~send_msg ]
    | None -> return_nil
  in

  let* content =
    match model.route with
    | Route.About -> return [ about_view ]
    | Route.Login -> return [ Login.view send_msg model.xmpp ]
    | Route.Activity ->
        return @@ Activity.view ~send_msg model.posts model.graph
    | Route.Map -> return @@ [ Mapg.view ~send_msg model.map ]
    | Route.Roster -> (
        match model.xmpp with
        | Loadable.Loaded xmpp ->
            return @@ [ Roster.view_roster ~send_msg xmpp model.roster ]
        | _ -> return @@ [])
    | Route.RosterItem jid -> (
        match model.xmpp with
        | Loadable.Loaded xmpp ->
            return @@ [ Roster.view_contact ~send_msg jid xmpp model.roster ]
        | _ -> return @@ [])
    | Route.AddContact -> return @@ [ Roster.view_add_contact ~send_msg ]
  in

  return (menu send_msg model :: (content @ action_bar))

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
  Logs.set_level @@ Some Logs.Debug;

  (* Initialize random generator *)
  Random.self_init ();

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
