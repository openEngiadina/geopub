(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Reactor_brr
open Brr
open Brr_io
open Lwt
module L = Loadable

(* XMPP PubSub *)

let compose_form send_msg _client =
  ignore send_msg;
  Evr.on_el ~default:false Form.Ev.submit
    (fun ev ->
      let form_data =
        Form.Data.of_form @@ Form.of_jv @@ Ev.target_to_jv @@ Ev.target ev
      in

      let post_content_value =
        Form.Data.find form_data (Jstr.v "post-content") |> Option.get
      in

      let post_content =
        match post_content_value with
        | `String js -> Jstr.to_string js
        | _ -> failwith "We need better error handling"
      in

      send_msg @@ `XmppMsg (Xmppg.PublishPost post_content))
    El.(
      form
        ~at:At.[ class' @@ Jstr.v "post-compose" ]
        [
          input
            ~at:At.[ type' @@ Jstr.v "text"; name @@ Jstr.v "post-content" ]
            ();
          input
            ~at:At.[ type' @@ Jstr.v "submit"; value @@ Jstr.v "Publish" ]
            ();
        ])

let view send_msg (model : Xmppg.model L.t) =
  match model with
  | L.Loaded model ->
      return
      @@ El.
           [
             div
               ~at:At.[ class' @@ Jstr.v "text-content" ]
               [ compose_form send_msg model.client ];
           ]
  | _ -> return_nil
