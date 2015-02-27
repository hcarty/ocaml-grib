open Grib

let () =
  let h = Handle.of_sample "GRIB2" in
  let f = G2clib.of_handle h in
  let v = G2clib.get_values f in
  let v' = Handle.get_double_array h "values" in
  assert (v = v');
  let _m1, _m2 = G2clib.get_missing f in
  Handle.delete h;
  ()
