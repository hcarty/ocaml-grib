type 'a field

(* Raw C interface *)

external _of_message : Message.t -> int -> unpack:bool -> expand:bool -> 'a field
  = "ml_g2_getfld"
external _of_handle : Handle.handle -> int -> unpack:bool -> expand:bool -> 'a field
  = "ml_g2_getfld_handle"

external get_ngrdpts : 'a field -> int = "ml_get_ngrdpts"

(* Usable wrappers *)

external get_missing : 'a field -> float option * float option = "ml_g2_miss"

external get_float_array :
  ?missing1:float ->
  ?missing2:float ->
  [ `unpacked ] field -> float array = "ml_get_data"

external get_float_array_into_ba :
  ?missing1:float ->
  ?missing2:float ->
  [ `unpacked ] field ->
  (float, Bigarray.float64_elt, 'a) Bigarray.Array1.t ->
  unit = "ml_get_data_ba"

let get_float_array_ba ?missing1 ?missing2 field layout =
  let open Bigarray in
  let n = get_ngrdpts field in
  let ba = Array1.create float64 layout n in
  get_float_array_into_ba ?missing1 ?missing2 field ba;
  ba

let of_message ?(field = 1) message =
  _of_message message field ~unpack:true ~expand:true

let metadata ?(field = 1) message =
  _of_message message field ~unpack:false ~expand:false

let of_handle ?(field = 1) handle =
  Handle.use (
    fun h ->
      _of_handle h field ~unpack:true ~expand:true
  ) handle

