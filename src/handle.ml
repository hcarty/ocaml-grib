open Bear

type handle
type file_t

type t = {
  handle : handle;
  mutable closed : bool;
}

exception Invalid_handle

(** [delete h] deletes the given GRIB handle [h]. *)
external delete : handle -> unit = "ml_grib_handle_delete"

let delete ({ handle; closed } as t) =
  if closed then (
    (* Nothing to do *)
  )
  else (
    delete handle;
    t.closed <- true;
  )

let init handle =
  let t = { handle; closed = false } in
  Gc.finalise delete t;
  t

let use f { handle; closed } =
  if closed then raise Invalid_handle;
  f handle

let use_1 f h a =
  use (fun h -> f h a) h
let use_2 f h a b =
  use (fun h -> f h a b) h

(** [get_size handle key] returns the number of elements associated with [key]
    in the GRIB [handle]. *)
external get_size : handle -> string -> int = "ml_grib_get_size"

let get_size = use_1 get_size

(** Type for generic getters/setters *)
type value_type_t =
  | String_type
  | Int_type
  | Float_type

type value_t =
  | String of string
  | Int of int
  | Float of float

(** [get_native_type handle key] returns the native type of the value
    associated with [key]._*)
external get_native_type : handle -> string -> value_type_t =
  "ml_grib_get_native_type"

let get_native_type = use_1 get_native_type

(** [get_double_array handle key] returns the elements from [handle] associated
    with [key] as floating point values. *)
external get_double_array : handle -> string -> float array =
  "ml_grib_get_double_array"

external get_double_array_ba :
  handle -> string ->
  (float, Bigarray.float64_elt, _) Bigarray.Array1.t ->
  unit = "ml_grib_get_double_array_ba"

let get_double_array_into_ba handle key data =
  let open Bigarray in
  if Array1.dim data <> get_size handle key then
    invalid_arg "get_double_array_into_ba";
  use_2 get_double_array_ba handle key data;
  ()

let get_double_array_ba handle key layout =
  let open Bigarray in
  let kind = float64 in
  let data = Array1.create kind layout (get_size handle key) in
  use_2 get_double_array_ba handle key data;
  data

(** [get_string_any handle key] returns the elements from [handle] associated
    with [key] as a string, regardless of the native type of [key]. *)
external get_string_any : handle -> string -> string = "ml_grib_get_string_any"

(** [get_string handle key] returns the elements from [handle] associated
    with [key] as a string. *)
external get_string : handle -> string -> string = "ml_grib_get_string"

(** [get_long handle key] returns the element from [handle] associated
    with [key] as an integer. *)
external get_long : handle -> string -> int = "ml_grib_get_long"

(** [get_double handle key] returns the element from [handle] associated
    with [key] as a float. *)
external get_double : handle -> string -> float = "ml_grib_get_double"

let get_double_array = use_1 get_double_array
let get_string_any = use_1 get_string_any
let get_string = use_1 get_string
let get_long = use_1 get_long
let get_double = use_1 get_double

(** Aliases *)
let get_float_array = get_double_array
let get_float_array_ba = get_double_array_ba
let get_float_array_into_ba = get_double_array_into_ba
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
  | String_type -> String (get_string h k)
  | Int_type -> Int (get_long h k)
  | Float_type -> Float (get_double h k)

let get_opt h k = get_opt_wrapper get h k

(** [get_message_copy handle] returns the message associated with [handle] as
    a string. *)
external get_message_copy : handle -> Message.t = "ml_grib_get_message_copy"

let get_message_copy = use get_message_copy

(** [set_long handle key x] sets [key] to [x] in [handle]. *)
external set_long : handle -> string -> int -> unit = "ml_grib_set_long"

(** [set_double handle key x] sets [key] to [x] in [handle]. *)
external set_double : handle -> string -> float -> unit = "ml_grib_set_double"

(** [set_string handle key x] sets [key] to [x] in [handle]. *)
external set_string : handle -> string -> string -> unit = "ml_grib_set_string"

(** [set_double_array handle key x] sets [key] to [x] in [handle]. *)
external set_double_array : handle -> string -> float array -> unit =
  "ml_grib_set_double_array"

external set_double_array_ba :
  handle -> string ->
  (float, Bigarray.float64_elt, _) Bigarray.Array1.t ->
  unit = "ml_grib_set_double_array_ba"

(** [set_long_array handle key x] sets [key] to [x] in [handle]. *)
external set_long_array : handle -> string -> int array -> unit =
  "ml_grib_set_long_array"

let set_long = use_2 set_long
let set_double = use_2 set_double
let set_string = use_2 set_string
let set_double_array = use_2 set_double_array
let set_double_array_ba h k v = use_2 set_double_array_ba h k v
let set_long_array = use_2 set_long_array

(** Aliases *)
let set_int = set_long
let set_float = set_double
let set_float_array = set_double_array
let set_float_array_ba = set_double_array_ba
let set_int_array = set_long_array

(** Generic setter *)
let set h k v =
  match v with
  | String s -> set_string h k s
  | Int i -> set_long h k i
  | Float f -> set_double h k f

(** [of_message message] returns a {!t} associated with [message].  [message]
    is copied in the process. *)
external of_message : Message.t -> handle =
  "ml_grib_handle_new_from_message_clone"

let of_message m = init (of_message m)

(** [of_sample name] returns a {!t} based on the sample [name]. *)
external of_sample : string -> handle = "ml_grib_handle_new_from_samples"

let of_sample f = init (of_sample f)

(** [map_sample f sample] applies [f] to the handle based on [sample].
    [sample] refers to one of the samples which come with the GRIB API. *)
let map_sample f sample =
  with_dispose ~dispose:delete f (of_sample sample)

(** [open_file name] opens the file [name] for reading. *)
external open_file : string -> file_t = "ml_grib_open_file"

(** [close_file file] closes a [file] opened with {!open_file}. *)
external close_file : file_t -> unit = "ml_grib_close_file"

(** [next_handle file] returns the next GRIB handle from [file]. *)
external next_handle : file_t -> handle option = "ml_grib_handle_new_from_file"

let next_handle file =
  Option.map init (next_handle file)

let fold f t accu_init =
  let rec loop accu =
    match next_handle t with
    | None -> accu
    | Some handle ->
      let result =
        with_dispose ~dispose:delete (fun h -> f h accu) handle
      in
      loop result
  in
  loop accu_init

(** [iter f t] iterates over each handle included in [t], applying [f]. *)
let iter f t =
  fold (fun h () -> f h) t ()

(** [map f t] applies [f] to each handle included in [t]. *)
let map f t =
  List.rev (fold (fun h l -> f h :: l) t [])

(** [filter_map f t] applies [f] to each handle included in [t], filtering the
    results as each handle is processed. *)
let filter_map f t =
  List.rev (
    fold (
      fun h l ->
        match f h with
        | Some result -> result :: l
        | None -> l
    ) t []
  )

(** Apply [f] to the file handle from [filename]. *)
let with_file_in filename f =
  with_dispose ~dispose:close_file f (open_file filename)

let fold_file f filename =
  with_file_in filename (fold f)

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

(** Iterating over keys *)

module Keys = struct
  (** Type for a key iterator *)
  type iterator_t

  (** Flags to limit which keys we iterate over *)
  type iterator_flag_t =
    | All_keys
    | Skip_read_only
    | Skip_optional
    | Skip_edition_specific
    | Skip_coded
    | Skip_computed
    | Skip_duplicates
    | Skip_function

  external iterator_new :
    handle -> iterator_flag_t list -> string option -> iterator_t =
    "ml_grib_keys_iterator_new"
  external iterator_next : iterator_t -> string option =
    "ml_grib_keys_iterator_next"
  external iterator_delete : iterator_t -> unit =
    "ml_grib_keys_iterator_delete"

  let iterator_new = use_2 iterator_new

  let map ?(flags = [All_keys]) ?namespace f h =
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

  let iter ?(flags = [All_keys]) ?namespace f h =
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

  let filter_map ?(flags = [All_keys]) ?namespace f h =
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
