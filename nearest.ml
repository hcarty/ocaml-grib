(** Functions for locating nearest neighbor points *)

open Batteries

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
  | SAME_POINT
  | SAME_GRID
  | SAME_DATA

external of_handle : Handle.t -> t = "ml_grib_nearest_new"

external _find :
  t -> Handle.t -> float -> float -> int ->
  (float array * float array * float array * float array * int array) =
  "ml_grib_nearest_find"
external _find_multiple :
  Handle.t -> bool -> float array -> float array ->
  (float array * float array * float array * float array * int array) =
  "ml_grib_nearest_find_multiple"

let int_of_flags l =
  let rec f flags accu =
    match flags with
    | [] -> accu
    | hd :: tl -> begin
      match hd with
      | SAME_POINT ->
          f tl (1 land accu)
      | SAME_GRID ->
          f tl (2 land accu)
      | SAME_DATA ->
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

