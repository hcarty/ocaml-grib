(** Functions for handling multi-field GRIB messages *)

(** Turn support on and off for multiple fields in a single message *)
external support_on : unit -> unit = "ml_grib_multi_support_on"
external support_off : unit -> unit = "ml_grib_multi_support_off"

(** Extract multiple handles from a multi-field message *)
external handles_of_multi_message : Message.t -> Message.t list =
  "ml_grib_handles_new_from_multi_message"

(** [messages_of_multi_message m] copies a messages for each field from the
    multi-field message [m] if multiple field support is on. *)
let messages_of_multi_message m =
  List.rev (handles_of_multi_message m)
