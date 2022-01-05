(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Brr_io
open Reactor_brr

let view send_msg =
  let login_form =
    El.(
      form
        [
          (* JID *)
          label ~at:At.[ for' @@ Jstr.v "jid" ] [ txt' "JID" ];
          input
            ~at:
              At.
                [
                  id @@ Jstr.v "jid";
                  name @@ Jstr.v "jid";
                  type' @@ Jstr.v "text";
                ]
            ();
          (* Password *)
          label ~at:At.[ for' @@ Jstr.v "password" ] [ txt' "Password" ];
          input
            ~at:
              At.
                [
                  id @@ Jstr.v "password";
                  name @@ Jstr.v "password";
                  type' @@ Jstr.v "password";
                ]
            ();
          (* Submit *)
          input
            ~at:
              At.
                [
                  id @@ Jstr.v "submit";
                  type' @@ Jstr.v "submit";
                  value @@ Jstr.v "Login";
                ]
            ();
        ])
  in
  El.(
    div
      ~at:At.[ id @@ Jstr.v "login"; class' @@ Jstr.v "text-content" ]
      [
        h1 [ txt' "Login" ];
        p [ txt' "You may login using any XMPP account." ];
        Evr.on_el ~default:false Form.Ev.submit
          (fun ev ->
            let form_data =
              Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
            in

            let jid_value =
              Form.Data.find form_data (Jstr.v "jid") |> Option.get
            in
            let password_value =
              Form.Data.find form_data (Jstr.v "password") |> Option.get
            in

            let jid =
              match jid_value with
              | `String js -> Jstr.to_string js |> Xmpp.Jid.of_string_exn
              | _ -> failwith "We need better error handling"
            in

            let password =
              match password_value with
              | `String js -> Jstr.to_string js
              | _ -> failwith "We need better error handling"
            in

            send_msg @@ `XmppMsg (Xmppg.Login (jid, password)))
          login_form;
        p
          [
            Evr.on_el Ev.click (fun _ -> send_msg (`XmppMsg LoginAnonymousDemo))
            @@ a
                 ~at:At.[ href @@ Jstr.v "#" ]
                 [ txt' "Login anonymously with demo.opengiadina.net" ];
          ];
      ])
