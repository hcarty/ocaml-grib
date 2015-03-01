open Grib
open OUnit2

let explicit _ctx =
  for _ = 1 to 10_000 do
    Handle.(of_sample "GRIB2" |> delete)
  done

let gc _ctx =
  for _ = 1 to 10_000 do
    ignore (Grib.Handle.of_sample "GRIB2");
    Gc.compact ()
  done

let t =
  "GC" >::: [
    "Explicit" >:: explicit;
    "GC" >:: gc;
  ]
