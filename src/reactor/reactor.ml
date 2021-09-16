(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Lwt
open Lwt_react

module Return = struct
  type ('model, 'cmd) t = 'model * 'cmd list

  let singleton model = (model, [])

  let command cmd (model, cmds) = (model, cmd :: cmds)

  let commands cmds_to_add (model, cmds) = (model, cmds_to_add @ cmds)

  let map f (model, cmds) = (f model, cmds)

  let map_cmd f (model, cmds) = (model, List.map f cmds)

  let bind f (model, cmds) =
    let model_f, cmds_f = f model in
    (model_f, cmds @ cmds_f)

  let run runner (model, cmds) =
    List.iter runner cmds;
    model
end

module App = struct
  type ('a, 'model, 'msg) t = {
    start : unit Lwt.u;
    send : ?step:React.step -> 'msg -> unit;
    model_signal : 'model Lwt_react.signal;
    result : ('a, exn) Lwt_result.t;
  }

  type ('model, 'msg) init = unit -> ('model, 'msg Lwt.t) Return.t

  type ('a, 'model, 'msg) update =
    stop:('a -> unit) ->
    send_msg:(?step:React.step -> 'msg -> unit) ->
    'model ->
    'msg ->
    ('model, 'msg Lwt.t) Return.t

  let create ~init ~update =
    (* set equality for model signal to phyisical equality *)
    let eq a b = a == b in

    (* message events *)
    let msg_e, send_msg = E.create () in

    (* command events *)
    let cmd_e, send_cmd = E.create () in

    (* app result and resolver *)
    let result, resolver = Lwt.task () in
    let on_exception e = Lwt.wakeup resolver (Error e) in
    let on_stop a = Lwt.wakeup resolver (Ok a) in

    (* run the commands and send results on as messages *)
    (* Note: we need to keep this from the garbage collector *)
    cmd_e |> E.map (fun t -> Lwt.on_any t send_msg on_exception) |> E.keep;

    (* we need the initial model but can not run initial effects
       before the msg handling is set up *)
    let wait_for_start, start = Lwt.wait () in
    let init_model =
      init ()
      |> Return.map_cmd (fun cmd -> wait_for_start >>= fun () -> cmd)
      |> Return.run send_cmd
    in

    (* run update function on message event *)
    (* Note: we need to use fold_s to ensure atomic updates *)
    let model_signal =
      S.fold_s ~eq
        (fun model msg ->
          try
            update ~stop:on_stop ~send_msg model msg
            |> Return.run send_cmd |> return
          with e ->
            on_exception e;
            model |> return)
        init_model msg_e
    in

    (* stop events and signal *)
    let result =
      result >>= fun value ->
      Lwt_react.E.stop msg_e;
      Lwt_react.E.stop cmd_e;
      Lwt_react.S.stop model_signal;
      return value
    in

    (* Catch any other possible exceptions (e.g. Cancel) *)
    let result = Lwt.catch (fun () -> result) (fun e -> return @@ Error e) in

    { start; send = send_msg; model_signal; result }

  let start t = Lwt.wakeup t.start ()

  let model t = t.model_signal

  let send t ?step msg = t.send ?step msg

  let result t = t.result
end
