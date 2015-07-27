open Bear

type t

external _new : Handle.handle -> int -> t = "ml_grib_iterator_new"

let of_handle handle =
  (* flags is unused by the GRIB API as of version 1.13.1, so this is a
     placeholder for now. *)
  let flags = 1 in
  Handle.use_1 _new handle flags

external next : t -> (float * float * float) option = "ml_grib_iterator_next"
external previous : t -> (float * float * float) option = "ml_grib_iterator_previous"
external reset : t -> unit = "ml_grib_iterator_reset"
external delete : t -> unit = "ml_grib_iterator_delete"

let iterator_in f h =
  with_dispose ~dispose:delete f (of_handle h)

let iter f h =
  iterator_in (
    fun i ->
      let has_next = ref true in
      while !has_next do
        match next i with
        | Some x -> f x
        | None -> has_next := false
      done;
  ) h

let map f h =
  iterator_in (
    fun i ->
      let rec inner l =
        match next i with
        | Some x -> inner (f x :: l)
        | None -> List.rev l
      in
      inner []
  ) h

let to_lat_lon_value h =
  let l = map identity h in
  let lats = List.map (fun (lat, _, _) -> lat) l in
  let lons = List.map (fun (_, lon, _) -> lon) l in
  let vs = List.map (fun (_, _, v) -> v) l in
  lats, lons, vs

