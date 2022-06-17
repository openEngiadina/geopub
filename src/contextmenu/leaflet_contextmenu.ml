(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

module Menu = struct
  type 'a item =
    | Callback of (string * (([> `Mouse ] as 'a) Leaflet.Event.t -> unit))
    | Seperator

  type t = [ `Mouse ] item list

  let to_v menu =
    Jv.of_list
      (function
        | Callback (text, callback) ->
            (* leaflet_contextmenu will gives a raw Jv.t on callback.
             * So we use [f] as an intermediary function to make it a nice [> `Mouse ] Leaflet.Event.t *)
            let f jv_t =
              jv_t |> Leaflet.Event.of_jv Leaflet.Event.Click |> callback
            in
            Jv.obj [| ("text", Jv.of_string text); ("callback", Jv.repr f) |]
        | Seperator -> Jv.obj [| ("separator", Jv.true') |])
      menu
end

let options menu =
  Jv.obj [| ("contextmenu", Jv.true'); ("contextmenuItems", Menu.to_v menu) |]
