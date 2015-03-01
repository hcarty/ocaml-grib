open Grib
open OUnit2

let iter _ctx =
  let h = Handle.of_sample "GRIB2" in
  Iterator.iter ignore h;
  ignore (Iterator.map ignore h);
  ignore (Iterator.to_lat_lon_value h);
  Handle.delete h;
  ()

let t =
  "Index" >::: [
    "iter" >:: iter
  ]
