open Bear

type index

type t = {
  index : index;
  mutable closed : bool;
}

type vt =
  | Double of float
  | Long of int
  | String of string

type kv = {
  k : string;
  v : vt;
}

exception Invalid_index

external create : string -> index = "ml_grib_index_new"
external from_file : string -> string -> index = "ml_grib_index_new_from_file"
external add_file : index -> string -> unit = "ml_grib_index_add_file"

external delete : index -> unit = "ml_grib_index_delete"

let delete ({ index; closed } as t) =
  if closed then (
    (* Nothing to do *)
  )
  else (
    delete index;
    t.closed <- true;
  )

let init index =
  let t = { index; closed = false } in
  Gc.finalise delete t;
  t

let create keys =
  let keys = String.concat "," keys in
  init (create keys)

let of_file filename keys =
  let keys = String.concat "," keys in
  init (from_file filename keys)

let use { index; closed } f =
  if closed then raise Invalid_index;
  f index

let add_file index filename =
  use index (fun i -> add_file i filename)

let of_files ~files ~keys =
  let i = create keys in
  List.iter (add_file i) files;
  i

external select_double : index -> string -> float -> unit = "ml_grib_index_select_double"
external select_long : index -> string -> int -> unit = "ml_grib_index_select_long"
external select_string : index -> string -> string -> unit = "ml_grib_index_select_string"

external size : index -> string -> int = "ml_grib_index_get_size"

external next_handle : index -> Handle.handle option = "ml_grib_handle_new_from_index"
let next_handle index =
  use index (
    fun i ->
      Option.map Handle.init (next_handle i)
  )

external write : index -> string -> unit = "ml_grib_index_write"
external read : string -> index = "ml_grib_index_read"

let write index filename =
  use index (fun i -> write i filename)

let read filename =
  init (read filename)

let double_key k v = { k; v = Double v }
let long_key k v = { k; v = Long v }
let string_key k v = { k; v = String v }

let float_key = double_key
let int_key = long_key

let select_float index k v =
  use index (fun i -> select_double i k v)

let select_int index k v =
  use index (fun i -> select_long i k v)

let select_string index k v =
  use index (fun i -> select_string i k v)

let select_one index { k; v } =
  match v with
  | Double d -> select_float index k d
  | Long l -> select_int index k l
  | String s -> select_string index k s

let select index kvs =
  List.iter (select_one index) kvs

let keys_of_kvs l =
  List.map (fun { k; _ } -> k) l

(** Fold [f] over each handle included in [t]. *)
let fold f t accu_init =
  let rec loop accu =
    match next_handle t with
    | None -> accu
    | Some handle ->
      loop (with_dispose ~dispose:Handle.delete (fun h -> f h accu) handle)
  in
  loop accu_init

(** Iterate [f] over each handle included in [t]. *)
let iter f t =
  fold (fun h () -> f h) t ()

(** Apply [f] to each handle included in [t]. *)
let map f t =
  List.rev (fold (fun h l -> f h :: l) t [])

(** Apply [f] on the index from [filename], optionally initializing the index
    with the (key, value) pairs from [init]. *)
let with_file_in ?init filename keys f =
  with_dispose ~dispose:delete (
    fun index ->
      Option.may (select index) init;
      f index
  ) (of_file filename keys)

let with_files_in ?init ~files ~keys f =
  with_dispose ~dispose:delete (
    fun index ->
      Option.may (select index) init;
      f index
  ) (of_files ~files ~keys)
