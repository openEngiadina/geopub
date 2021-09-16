(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt.Syntax
open Lwt_react
open Brr
open Brr_io
open Brr_react
module Client = Xmpp_websocket.Client

type action = Login of Xmpp.Jid.t * string | NoOp

type t = string option

module Login = struct
  let ui () =
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

    let e =
      Evr.on_el ~default:false Form.Ev.submit
        (fun ev ->
          Ev.prevent_default ev;

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

          Login (jid, password))
        login_form
    in

    (e, [ login_form ])
end

let update model action =
  match action with
  | Login (jid, password) ->
      let* client =
        Client.create
          { ws_endpoint = Some "ws://localhost:5280/xmpp-websocket" }
          jid ~password
      in
      let* () = Client.connect client in
      return_some "hi"
  | NoOp -> return model

let init () = None

let ui model_s =
  let container =
    El.(div ~at:At.[ id @@ Jstr.v "messaging" ] [ txt' "XMPP" ])
  in
  let act_el_s =
    S.map ~eq:( == )
      (function
        | None -> Login.ui ()
        | Some _client ->
            let actions_e, _ = E.create () in
            (actions_e, El.[ txt' "Hi" ]))
      model_s
  in
  let act =
    S.bind ~eq:( == ) act_el_s (fun (act, _) -> S.hold NoOp act) |> S.changes
  in
  Elr.def_children container S.(map ~eq:( == ) snd act_el_s) |> S.keep;
  (act, container)
