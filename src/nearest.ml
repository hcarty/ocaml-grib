(** Functions for locating nearest neighbor points *)

(** A nearest *)
type t

(** A nearby point *)
type near_t = {
  target : (float * float);
  loc : (float * float);
  value : float;
  distance : float;
  index : int;
}

(** Flags for {!find} *)
type flag_t =
  | Same_point
  | Same_grid
  | Same_data

external of_handle : Handle.handle -> t = "ml_grib_nearest_new"
let of_handle h =
  Handle.use of_handle h

external _find :
  t -> Handle.handle -> float -> float -> int ->
  (float array * float array * float array * float array * int array) =
  "ml_grib_nearest_find"
let _find t handle x y i =
  Handle.use (fun h -> _find t h x y i) handle
external _find_multiple :
  Handle.handle -> bool -> float array -> float array ->
  (float array * float array * float array * float array * int array) =
  "ml_grib_nearest_find_multiple"
let _find_multiple handle b xs ys =
  Handle.use (fun h -> _find_multiple h b xs ys) handle

let int_of_flags l =
  let rec f flags accu =
    match flags with
    | [] -> accu
    | hd :: tl -> begin
      match hd with
      | Same_point ->
          f tl (1 land accu)
      | Same_grid ->
          f tl (2 land accu)
      | Same_data ->
          f tl (4 land accu)
    end
  in
  f l 0

(** Find a the four nearest points to a given [(lon, lat)]. *)
let find ?(flags = []) n handle ((lon, lat) as target) =
  let flag_int = int_of_flags flags in
  let (nlats, nlons, nvalues, ndistances, nindexes) =
    _find n handle lat lon flag_int
  in
  let expected_elements = 4 in
  assert (Array.length nlats = expected_elements);
  assert (Array.length nlons = expected_elements);
  assert (Array.length nvalues = expected_elements);
  assert (Array.length ndistances = expected_elements);
  assert (Array.length nindexes = expected_elements);
  Array.init expected_elements (
    fun i ->
      {
        target;
        loc = (nlons.(i), nlats.(i));
        value = nvalues.(i);
        distance = ndistances.(i);
        index = nindexes.(i);
      }
  )

(** Find the nearest point to each of a series of data points *)
let find_multiple ?(mask = false) handle points =
  let lons = Array.make (Array.length points) 0.0 in
  let lats = Array.make (Array.length points) 0.0 in
  Array.iteri (
    fun i (lon, lat) ->
      lons.(i) <- lon;
      lats.(i) <- lat;
  ) points;
  let (nlats, nlons, nvalues, ndistances, nindexes) =
    _find_multiple handle mask lats lons
  in
  let expected_elements = Array.length points in
  assert (Array.length nlats = expected_elements);
  assert (Array.length nlons = expected_elements);
  assert (Array.length nvalues = expected_elements);
  assert (Array.length ndistances = expected_elements);
  assert (Array.length nindexes = expected_elements);
  Array.init expected_elements (
    fun i ->
      {
        target = points.(i);
        loc = (nlons.(i), nlats.(i));
        value = nvalues.(i);
        distance = ndistances.(i);
        index = nindexes.(i);
      }
  )

