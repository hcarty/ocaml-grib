open Batteries

type t
type file_t

(** [delete h] deletes the given GRIB handle [h]. *)
external delete : t -> unit = "ml_grib_handle_delete"

(** [get_size handle key] returns the number of elements associated with [key]
    in the GRIB [handle]. *)
external get_size : t -> string -> int = "ml_grib_get_size"

(** Type for generic getters/setters *)
type value_type_t =
  | TYPE_STRING
  | TYPE_LONG
  | TYPE_DOUBLE

type value_t =
  | String of string
  | Int of int
  | Float of float

(** [get_native_type handle key] returns the native type of the value
    associated with [key]._*)
external get_native_type : t -> string -> value_type_t =
  "ml_grib_get_native_type"

(** [get_double_array handle key] returns the elements from [handle] associated
    with [key] as floating point values. *)
external get_double_array : t -> string -> float array =
  "ml_grib_get_double_array"

(** [get_string_any handle key] returns the elements from [handle] associated
    with [key] as a string, regardless of the native type of [key]. *)
external get_string_any : t -> string -> string = "ml_grib_get_string_any"

(** [get_string handle key] returns the elements from [handle] associated
    with [key] as a string. *)
external get_string : t -> string -> string = "ml_grib_get_string"

(** [get_long handle key] returns the element from [handle] associated
    with [key] as an integer. *)
external get_long : t -> string -> int = "ml_grib_get_long"

(** [get_double handle key] returns the element from [handle] associated
    with [key] as a float. *)
external get_double : t -> string -> float = "ml_grib_get_double"

(** Aliases *)
let get_float_array = get_double_array
let get_as_string = get_string_any
let get_int = get_long
let get_float = get_double

(** Optional getters *)
let get_opt_wrapper f h k =
  try 
    Some (f h k)
  with
  | Invalid_argument _ ->
      None

let get_float_array_opt h k = get_opt_wrapper get_float_array h k
let get_string_opt h k = get_opt_wrapper get_string h k
let get_int_opt h k = get_opt_wrapper get_int h k
let get_float_opt h k = get_opt_wrapper get_float h k
let get_as_string_opt h k = get_opt_wrapper get_as_string h k

(** Generic getters *)
let get h k =
  match get_native_type h k with
  | TYPE_STRING -> String (get_string h k)
  | TYPE_LONG -> Int (get_long h k)
  | TYPE_DOUBLE -> Float (get_double h k)

let get_opt h k = get_opt_wrapper get h k

(** [get_message_copy handle] returns the message associated with [handle] as
    a string. *)
external get_message_copy : t -> Message.t = "ml_grib_get_message_copy"

(** [set_long handle key x] sets [key] to [x] in [handle]. *)
external set_long : t -> string -> int -> unit = "ml_grib_set_long"

(** [set_double handle key x] sets [key] to [x] in [handle]. *)
external set_double : t -> string -> float -> unit = "ml_grib_set_double"

(** [set_string handle key x] sets [key] to [x] in [handle]. *)
external set_string : t -> string -> string -> unit = "ml_grib_set_string"

(** [set_double_array handle key x] sets [key] to [x] in [handle]. *)
external set_double_array : t -> string -> float array -> unit =
  "ml_grib_set_double_array"

(** [set_long_array handle key x] sets [key] to [x] in [handle]. *)
external set_long_array : t -> string -> int array -> unit =
  "ml_grib_set_long_array"

(** Aliases *)
let set_int = set_long
let set_float = set_double
let set_float_array = set_double_array

(** Generic setter *)
let set h k v =
  match v with
  | String s -> set_string h k s
  | Int i -> set_long h k i
  | Float f -> set_double h k f

(** [of_message message] returns a {!t} associated with [message].  [message]
    is copied in the process. *)
external of_message : Message.t -> t =
  "ml_grib_handle_new_from_message_clone"

(** [of_sample name] returns a {!t} based on the sample [name]. *)
external of_sample : string -> t = "ml_grib_handle_new_from_samples"

(** [map_sample f sample] applies [f] to the handle based on [sample].
    [sample] refers to one of the samples which come with the GRIB API. *)
let map_sample f sample =
  with_dispose ~dispose:delete f (of_sample sample)

(** [open_file name] opens the file [name] for reading. *)
external open_file : string -> file_t = "ml_grib_open_file"

(** [close_file file] closes a [file] opened with {!open_file}. *)
external close_file : file_t -> unit = "ml_grib_close_file"

(** [next_handle file] returns the next GRIB handle from [file]. *)
external next_handle : file_t -> t option = "ml_grib_handle_new_from_file"

(** [iter f t] iterates over each handle included in [t], applying [f]. *)
let iter f t =
  let continue = ref true in
  while !continue do
    match next_handle t with
    | Some handle ->
        with_dispose ~dispose:delete f handle
    | None ->
        (* Break out of the loop when we are out of handles *)
        continue := false;
  done;
  ()

(** [map f t] applies [f] to each handle included in [t]. *)
let map f t =
  let rec inner l =
    match next_handle t with
    | Some handle ->
        let result = with_dispose ~dispose:delete f handle in
        inner (result :: l)
    | None -> List.rev l
  in
  inner []

(** [filter_map f t] applies [f] to each handle included in [t], filtering the
    results as each handle is processed. *)
let filter_map f t =
  let rec inner l =
    match next_handle t with
    | Some handle ->
        let result = with_dispose ~dispose:delete f handle in
        (* Drop the entry if [f handle] returns [None] *)
        let new_l =
          match result with
          | Some r -> r :: l
          | None -> l
        in
        inner new_l
    | None -> List.rev l
  in
  inner []

(** Apply [f] to the file handle from [filename]. *)
let with_file_in filename f =
  with_dispose ~dispose:close_file f (open_file filename)

(** Like [iter], but starting with a file *)
let iter_file f filename =
  with_file_in filename (iter f)

(** Like [map]. but starting with a file *)
let map_file f filename =
  with_file_in filename (map f)

(** Like [filter_map], but starting with a file *)
let filter_map_file f filename =
  with_file_in filename (filter_map f)

(** [map_message f m] applies [f] to the {!Handle.t} associated with the
    message [m]. *)
let map_message f m =
  with_dispose ~dispose:delete f (of_message m)

(** [apply_message f m] applies [f] to the {!Handle.t} associated with the
    message [m]. *)
let apply_message f m =
  with_dispose ~dispose:delete f (of_message m)

(** Iterating over keys *)

module Keys = struct
  (** Type for a key iterator *)
  type iterator_t

  (** Flags to limit which keys we iterate over *)
  type iterator_flag_t =
    | ALL_KEYS
    | SKIP_READ_ONLY
    | SKIP_OPTIONAL
    | SKIP_EDITION_SPECIFIC
    | SKIP_CODED
    | SKIP_COMPUTED
    | SKIP_DUPLICATES
    | SKIP_FUNCTION

  external iterator_new :
    t -> iterator_flag_t list -> string option -> iterator_t =
    "ml_grib_keys_iterator_new"
  external iterator_next : iterator_t -> string option =
    "ml_grib_keys_iterator_next"
  external iterator_delete : iterator_t -> unit =
    "ml_grib_keys_iterator_delete"

  let map ?(flags = [ALL_KEYS]) ?namespace f h =
    with_dispose ~dispose:iterator_delete (
      fun iterator ->
        let rec inner l =
          match iterator_next iterator with
          | Some key ->
              let result = f key in
              inner (result :: l)
          | None ->
              List.rev l
        in
        inner []
    ) (iterator_new h flags namespace)

  let iter ?(flags = [ALL_KEYS]) ?namespace f h =
    with_dispose ~dispose:iterator_delete (
      fun iterator ->
        let continue = ref true in
        while !continue do
          match iterator_next iterator with
          | Some key ->
              f key
          | None ->
              continue := false
        done
    ) (iterator_new h flags namespace)

  let filter_map ?(flags = [ALL_KEYS]) ?namespace f h =
    with_dispose ~dispose:iterator_delete (
      fun iterator ->
        let rec inner l =
          match iterator_next iterator with
          | Some key ->
              (* Only add to the list if (f key) <> None *)
              let new_l =
                match f key with
                | Some result -> result :: l
                | None -> l
              in
              inner new_l
          | None ->
              List.rev l
        in
        inner []
    ) (iterator_new h flags namespace)
end
