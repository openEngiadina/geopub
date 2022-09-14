(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr

let view jid_opt current_route =
  let entry name route =
    El.(
      li
        ~at:
          At.(add_if (current_route = route) (class' @@ Jstr.v "uk-active") [])
        [ a ~at:[ Route.href route ] [ txt' name ] ])
  in
  El.(
    nav
      ~at:
        At.
          [
            class' @@ Jstr.v "uk-navbar-container"; true' @@ Jstr.v "uk-navbar";
          ]
      [
        (* Logo *)
        a
          ~at:
            At.
              [
                class' @@ Jstr.v "uk-navbar-item";
                class' @@ Jstr.v "uk-logo";
                UIKit.Margin.left;
                Route.(href About);
              ]
          [
            img
              ~at:At.[ id @@ Jstr.v "sgraffito"; src (Jstr.v "sgraffito.svg") ]
              ();
          ];
        (* left navigation *)
        div
          ~at:At.[ class' @@ Jstr.v "uk-navbar-left"; UIKit.Margin.left ]
          [
            ul
              ~at:At.[ class' @@ Jstr.v "uk-navbar-nav" ]
              [
                entry "Activity" (Route.Activity None);
                entry "Map" Route.Map;
                entry "Query" (Route.Query "triple-fts(?s,?p,?o, \"Hello\")");
              ];
          ];
        (* right navigation *)
        div
          ~at:At.[ class' @@ Jstr.v "uk-navbar-right"; UIKit.Margin.right ]
          [
            (match jid_opt with
            | Some jid ->
                ul
                  ~at:At.[ class' @@ Jstr.v "uk-navbar-nav" ]
                  [ entry Xmpp.Jid.(to_string @@ bare jid) Route.User ]
            | None ->
                ul
                  ~at:At.[ class' @@ Jstr.v "uk-navbar-nav" ]
                  [ entry "Login" Route.User ]);
          ];
      ])
