type 'a field_t

external _of_message : Message.t -> int -> unpack:bool -> expand:bool -> 'a field_t = "ml_g2_getfld"

external get_data : [ `unpacked ] field_t -> float array = "ml_get_data"

let of_message message i =
  _of_message message i ~unpack:true ~expand:true

let metadata message i =
  _of_message message i ~unpack:false ~expand:false

