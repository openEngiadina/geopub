(*
 * SPDX-FileCopyrightText: 2021, 2022 pukkamustard <pukkamustard@posteo.net>
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
    (* app result and resolver *)
    let result, resolver = Lwt.task () in
    let on_exception e = Lwt.wakeup resolver (Error e) in
    let on_stop a = Lwt.wakeup resolver (Ok a) in

    let msg_e, send_msg = Lwt_stream.create () in

    (* let msg_stream, send_msg = Lwt_stream.create () in *)
    let run_effects (model, cmds) =
      List.iter
        (fun cmd ->
          Lwt.on_any cmd (fun msg -> send_msg @@ Some msg) on_exception)
        cmds;
      model
    in

    let run_subscriptions (model, old_subscriptions) =
      (* TODO is this atomic? Could a message be dropped while switching over? Probably shoud use E.switch... *)
      (* strong stop is only for JavaScript. How to unset if not in Javascript land? *)
      E.stop ~strong:true old_subscriptions;
      (model, subscriptions model |> E.map (fun msg -> send_msg @@ Some msg))
    in

    (* run the initial effects *)
    let init_model = run_effects @@ init () in

    let model =
      S.fix ~eq:( == )
        ((init_model, E.never) |> run_subscriptions |> Return.singleton)
        (fun return_s ->
          let msg_e = E.of_stream msg_e in

          let update_e =
            E.map
              (fun msg return ->
                Return.bind
                  (fun (model, subscription_e) ->
                    update ~stop:on_stop model msg
                    |> Return.map (fun model -> (model, subscription_e)))
                  return
                |> run_effects |> run_subscriptions |> Return.singleton)
              msg_e
          in
          let return_s = S.accum ~eq:( == ) update_e (S.value return_s) in

          ( return_s,
            S.map ~eq:( == )
              (fun ((model, _subscription_e), _cmds) -> model)
              return_s ))
    in

    (* stop the msg events when resolving return *)
    let result =
      result >>= fun value ->
      send_msg None;
      return value
    in

    { send = (fun msg -> send_msg (Some msg)); model; result }

  let model t = t.model
  let send t msg = t.send msg
  let result t = t.result
end
