(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Lwt_react
open Brr
open Archi_lwt

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
        nav
          [
            ul
              [
                entry "Activity" (Route.Activity None);
                entry "Map" Route.Map;
                entry "Query" (Route.Query "triple-fts(?s,?p,?o, \"Hello\")");
              ];
          ];
        div ~at:At.[ class' @@ Jstr.v "spacer" ] [];
        nav
          [ ul [ entry "About" Route.About; entry "Settings" Route.Settings ] ];
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

let loading msg =
  El.
    [
      div
        ~at:At.[ class' @@ Jstr.v "loading" ]
        [ img ~at:At.[ src (Jstr.v "sgraffito.svg") ] (); p [ txt' msg ] ];
    ]

let about =
  El.(
    div
      ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "text-content" ]
      [
        h1 [ txt' "GeoPub" ];
        p [ txt' "Version 0.7.0-dev" ];
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

(* Component *)

type t = Model.t

let view t =
  t.router
  |> S.map_s ~eq:( = ) (function
       | Route.About -> return [ geopub_menu t; about ]
       | Route.Map -> return [ geopub_menu t; Geopub_map.view t.map ]
       | route -> return @@ loading (route |> Route.to_jstr |> Jstr.to_string))

(* match Router.current t.router with
 * | Route.About -> return [ Ui.geopub_menu model; Ui.about ]
 * | Route.Activity latlng ->
 *     let* activity = Activity.view ?latlng ~update model in
 *     return [ Ui.geopub_menu model; activity ]
 * | Route.Map ->
 *     let* map = Geopub_map.view model.map in
 *     return [ Ui.geopub_menu model; map ]
 * | Route.Query query ->
 *     let* query_view = Query.view model query in
 *     return [ Ui.geopub_menu model; query_view ]
 * | Route.Inspect iri -> Inspect.view model iri
 * | Route.Settings -> Settings.view ~update model *)

let start () router database xmpp_connection map :
    (t, [ `Msg of string ]) Result.t Lwt.t =
  let model : Model.t = { router; database; xmpp_connection; map } in

  (* Set the UI on the document body *)
  let body = Document.body G.document in
  let* () = view model >|= Brr_react.Elr.def_children body >|= S.keep in

  return_ok model

let stop _ = return_unit

let component =
  Component.using ~start ~stop
    ~dependencies:
      [
        Router.component;
        Database.component;
        Xmpp.Connection.component;
        Geopub_map.component;
      ]
