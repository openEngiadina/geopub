(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

open Brr
open Lwt

let view model =
  ignore model;
  return
  @@ El.(
       div
         ~at:At.[ id @@ Jstr.v "main"; class' @@ Jstr.v "content" ]
         [ h1 [ txt' "Query" ] ])
