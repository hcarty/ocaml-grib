type field_t

external of_message : Message.t -> int -> unpack:bool -> expand:bool -> field_t = "ml_g2_getfld"

external get_data : field_t -> float array = "ml_get_data"

