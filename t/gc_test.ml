let () =
  print_endline "Explicit delete";
  for _ = 1 to 10_000 do
    Grib.Handle.(of_sample "GRIB2" |> delete)
  done;
  print_endline "Relying on the GC";
  for _ = 1 to 10_000 do
    ignore (Grib.Handle.of_sample "GRIB2");
    Gc.compact ()
  done
