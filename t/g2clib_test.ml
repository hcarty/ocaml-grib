open Grib
open OUnit2

let getters _ctx =
  let h = Handle.of_sample "GRIB2" in
  let f = G2clib.of_handle h in
  let v = G2clib.get_float_array f in
  let v' = Handle.get_float_array h "values" in
  assert_bool "G2clib.get_values and Handle.get_float_array" (v = v');
  let _, _ = G2clib.get_missing f in
  assert_bool "G2clib.get_missing" true;
  Handle.delete h;
  ()

let t =
  "G2clib" >::: [
    "getters" >:: getters
  ]
