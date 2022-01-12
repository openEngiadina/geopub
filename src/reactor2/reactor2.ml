(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt_react

module Return = struct
  type ('model, 'msg) t = 'model * 'msg Lwt.t list

  let singleton model = (model, [])
  let command cmd (model, cmds) = (model, cmd :: cmds)
  let commands cmds_to_add (model, cmds) = (model, cmds_to_add @ cmds)
  let map f (model, cmds) = (f model, cmds)
  let map_cmd f (model, cmds) = (model, List.map (fun p -> Lwt.map f p) cmds)

  let bind f (model, cmds) =
    let model_f, cmds_f = f model in
    (model_f, cmds @ cmds_f)
end

module App = struct
  type ('a, 'model, 'msg) t = {
    send : 'msg -> unit;
    model : 'model S.t;
    result : ('a, exn) Lwt_result.t;
  }

  type ('model, 'msg) init = unit -> ('model, 'msg) Return.t

  type ('a, 'model, 'msg) update =
    stop:('a -> unit) -> 'model -> 'msg -> ('model, 'msg) Return.t

  type ('a, 'model, 'msg) subscriptions = 'model -> 'msg E.t

  let create ~init ~update ~subscriptions =
    ignore subscriptions;

    (* app result and resolver *)
    let result, resolver = Lwt.task () in
    let on_exception e = Lwt.wakeup resolver (Error e) in
    let on_stop a = Lwt.wakeup resolver (Ok a) in

    let msg_e, send_msg = E.create () in

    (* let msg_stream, send_msg = Lwt_stream.create () in *)
    let run_effects (model, cmds) =
      List.iter
        (fun cmd -> Lwt.on_any cmd (fun msg -> send_msg msg) on_exception)
        cmds;
      model
    in

    (* run the initial effects *)
    let init_model = run_effects @@ init () in

    let model =
      S.fix ~eq:( == ) (Return.singleton init_model) (fun return_s ->
          let update_e =
            E.map
              (fun msg return ->
                Return.bind (fun model -> update ~stop:on_stop model msg) return
                |> run_effects |> Return.singleton)
              msg_e
          in
          let return_s = S.accum update_e (S.value return_s) in
          (return_s, S.map fst return_s))
    in

    (* stop the msg events when resolving return *)
    let result =
      result >>= fun value ->
      E.stop msg_e;
      return value
    in

    { send = (fun msg -> send_msg ?step:None msg); model; result }

  let model t = t.model
  let send t msg = t.send msg
  let result t = t.result
end
