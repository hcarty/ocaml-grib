(** Module for working with GRIB messages *)

open Bear
open Bigarray

type t = bytes

let of_string s =
  Bytes.of_string s
let to_string m =
  Bytes.to_string m
external of_bytes : bytes -> t = "%identity"
external to_bytes : t -> bytes = "%identity"

let of_bigarray ba =
  Bytes.init (Array1.dim ba) (fun i -> Char.chr (Array1.unsafe_get ba i))

let to_bigarray m =
  let ba = Array1.create int8_unsigned c_layout (Bytes.length m) in
  Bytes.iteri (
    fun i c ->
      Array1.unsafe_set ba i (Char.code c)
  ) m;
  ba

let save_list mode perm filename m =
  match m with
  | [] -> () (* No messages, nothing to do *)
  | l ->
    with_dispose ~dispose:close_out
      (fun oc -> List.iter (output_bytes oc) l)
      (open_out_gen mode perm filename)

let save mode perm filename m =
  save_list mode perm filename [m]
