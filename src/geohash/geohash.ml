(*
 * SPDX-FileCopyrightText: 2022 pukkamustard <pukkamustard@posteo.net>
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *)

(* Alphabet *)

module Base32 = struct
  let alphabet = "0123456789bcdefghjkmnpqrstuvwxyz"
  let encode_map = Array.of_seq @@ String.to_seq alphabet

  let decode_map =
    let decode_map = Array.make 255 (-1) in
    String.iteri (fun i c -> decode_map.(Char.code c) <- i) alphabet;
    decode_map

  let to_bits byte =
    Seq.init 5 (fun i -> Int.(shift_right 0b10000 i land byte) > 0)

  let decode s =
    String.to_seq s
    |> Seq.map (fun char -> Array.get decode_map (Char.code char))
    |> Seq.concat_map to_bits

  let rec encode bits () =
    match List.of_seq @@ Seq.take 5 bits with
    | [ d0; d1; d2; d3; d4 ] ->
        let d =
          Int.(
            shift_left (Bool.to_int d0) 4
            lor shift_left (Bool.to_int d1) 3
            lor shift_left (Bool.to_int d2) 2
            lor shift_left (Bool.to_int d3) 1
            lor Bool.to_int d4)
        in
        let char = Array.get encode_map d in
        Seq.Cons (char, encode @@ Seq.drop 5 bits)
    | _ ->
        failwith
          "Internal error while encoding Base32. Expecting Seq to be infinite."
end

(* Decode *)

let find_interval (lower, upper, precision) bit =
  let midpoint = (lower +. upper) /. 2.0 in
  let precision' = precision /. 2.0 in
  if bit then (midpoint, upper, precision') else (lower, midpoint, precision')

let decode s =
  let lon_seq, lat_seq =
    (* decode Base32 encoding *)
    Base32.decode s
    (* split into longitude and latitude codes *)
    |> Seq.zip (Seq.cycle @@ List.to_seq [ true; false ])
    |> Seq.partition_map (fun (is_odd, bit) ->
           if is_odd then Either.left bit else Either.right bit)
  in
  let lat_l, lat_u, lat_e =
    Seq.fold_left find_interval (-90.0, 90.0, 90.0) lat_seq
  in
  let lat = (lat_l +. lat_u) /. 2.0 in

  let lng_l, lng_u, lng_e =
    Seq.fold_left find_interval (-180.0, 180.0, 180.0) lon_seq
  in
  let lng = (lng_l +. lng_u) /. 2.0 in

  ((lat, lat_e), (lng, lng_e))

(* Encode *)

let rec interval (lower, upper) v () =
  let midpoint = (lower +. upper) /. 2.0 in
  if v < midpoint then Seq.Cons (false, interval (lower, midpoint) v)
  else Seq.Cons (true, interval (midpoint, upper) v)

let encode ?(precision = 12) (lat, lng) =
  Seq.interleave (interval (-180.0, 180.0) lng) (interval (-90.0, 90.0) lat)
  |> Base32.encode |> Seq.take precision |> String.of_seq
