open Grib

let () =
  let h = Handle.of_sample "GRIB2" in
  Iterator.iter ignore h;
  ignore (Iterator.map ignore h);
  ignore (Iterator.to_lat_lon_value h);
  Handle.delete h;
  ()
