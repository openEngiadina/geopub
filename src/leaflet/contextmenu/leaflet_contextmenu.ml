(*
 * SPDX-FileCopyrightText: 2021 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

module Menu = struct
  type item =
    | Callback of (string * (Leaflet.Ev.MouseEvent.t -> unit))
    | Seperator

  type t = item list

  let to_v menu =
    Jv.of_list
      (function
        | Callback (text, callback) ->
            Jv.obj
              [| ("text", Jv.of_string text); ("callback", Jv.repr callback) |]
        | Seperator -> Jv.obj [| ("separator", Jv.true') |])
      menu
end

let options menu =
  Jv.obj [| ("contextmenu", Jv.true'); ("contextmenuItems", Menu.to_v menu) |]
