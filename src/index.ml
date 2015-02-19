open Batteries

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

external select_double : index -> string -> float -> unit = "ml_grib_index_select_double"
external select_long : index -> string -> int -> unit = "ml_grib_index_select_long"
external select_string : index -> string -> string -> unit = "ml_grib_index_select_string"

external size : index -> string -> int = "ml_grib_index_get_size"

external next_handle : index -> Handle.handle option = "ml_grib_handle_new_from_index"
let next_handle index =
  use index (fun i -> Handle.init (next_handle i))

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

let select index { k; v } =
  match v with
  | Double d -> select_float index k d
  | Long l -> select_int index k l
  | String s -> select_string index k s

let keys_of_kvs l =
  List.map (fun { k; _ } -> k) l

let apply_kvs index l =
  List.iter (select index) l

(** Fold [f] over each handle included in [t]. *)
let fold ?kvs f t accu_init =
  Option.may (apply_kvs t) kvs;
  let rec loop accu =
    match next_handle t with
    | None -> accu
    | Some handle ->
      with_dispose ~dispose:Handle.delete (fun h -> f h accu) handle
  in
  loop accu_init

(** Iterate [f] over each handle included in [t]. *)
let iter ?kvs f t =
  fold ?kvs (fun h () -> f h) t ()

(** Apply [f] to each handle included in [t]. *)
let map ?kvs f t =
  List.rev (fold ?kvs (fun h l -> f h :: l) t [])

(** Apply [f] on the index from [filename], optionally initializing the index
    with the (key, value) pairs from [init]. *)
let with_file_in ?init filename keys f =
  with_dispose ~dispose:delete (
    fun index ->
      Option.may (apply_kvs index) init;
      f index
  ) (of_file filename keys)

(** Like [iter], but starting with a file *)
let iter_file f filename init =
  let keys = keys_of_kvs init in
  with_file_in ~init filename keys (iter f)

(** Like [map]. but starting with a file *)
let map_file f filename init =
  let keys = keys_of_kvs init in
  with_file_in ~init filename keys (map f)

(** [get index kvs f] applies [f] to the handle matched by [kvs] in [index]. *)
let get index kvs f =
  apply_kvs index kvs;
  let matching_fields = map f index in
  match matching_fields with
  | [] -> None
  | hd :: [] -> Some hd
  | _ -> invalid_arg "Multiple results from GRIB index"

(** [get_exn index kvs f] is like {!get} but raises an exception rather than
    returning [None].

    @raise Not_found if no messages matching [kvs] are in [index]. *)
let get_exn index kvs f =
  match get index kvs f with
  | Some x -> x
  | None -> raise Not_found
