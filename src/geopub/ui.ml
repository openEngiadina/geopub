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

let loading =
  El.
    [
      div
        ~at:[ UIKit.container; UIKit.Position.center ]
        [
          img
            ~at:
              At.
                [
                  id @@ Jstr.v "sgraffito-large";
                  src (Jstr.v "sgraffito.svg");
                  UIKit.Align.center;
                ]
            ();
          p [ txt' "Loading GeoPub..." ];
        ];
    ]

let about =
  El.(
    div
      ~at:[ UIKit.container; UIKit.margin ]
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

let view (t : Model.t) =
  S.bind_s ~eq:( = ) t.router (fun current_route ->
      let with_navbar = S.map (fun c -> Navbar.view current_route :: c) in
      match current_route with
      | Route.About -> return @@ with_navbar @@ S.const [ about ]
      | Route.User -> User.view t.user >|= with_navbar
      | Route.Map -> return @@ with_navbar @@ S.const [ Geopub_map.view t.map ]
      | Route.Inspect iri -> Inspect.view t iri >|= with_navbar
      | _ -> return @@ with_navbar @@ S.const @@ loading)

(* match Router.current t.router with
 * | Route.Activity latlng ->
 *     let* activity = Activity.view ?latlng ~update model in
 *     return [ Ui.geopub_menu model; activity ]
 * | Route.Query query ->
 *     let* query_view = Query.view model query in
 *     return [ Ui.geopub_menu model; query_view ]
 * | Route.Settings -> Settings.view ~update model *)

let start () router database user map : (t, [ `Msg of string ]) Result.t Lwt.t =
  let model : Model.t = { router; database; user; map } in

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
        User.component;
        Geopub_map.component;
      ]
