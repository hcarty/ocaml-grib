type 'a field_t

(* Raw C interface *)

external _of_message : Message.t -> int -> unpack:bool -> expand:bool -> 'a field_t
  = "ml_g2_getfld"
external _of_handle : Handle.handle -> int -> unpack:bool -> expand:bool -> 'a field_t
  = "ml_g2_getfld_handle"

(* Usable wrappers *)

external get_missing : 'a field_t -> float option * float option = "ml_g2_miss"

external get_values :
  ?missing1:float ->
  ?missing2:float ->
  [ `unpacked ] field_t -> float array = "ml_get_data"

let of_message ?(field = 1) message =
  _of_message message field ~unpack:true ~expand:true

let metadata ?(field = 1) message =
  _of_message message field ~unpack:false ~expand:false

let of_handle ?(field = 1) handle =
  Handle.use (
    fun h ->
      _of_handle h field ~unpack:true ~expand:true
  ) handle

