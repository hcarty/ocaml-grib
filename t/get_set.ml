open Printf

let () =
  let h = Grib.Handle.of_sample "GRIB2" in
  let edition_number = Grib.Handle.get_int h "editionNumber" in
  let name = Grib.Handle.get_string h "name" in
  let average = Grib.Handle.get_float h "average" in
  let ni = Grib.Handle.get_int h "Ni" in
  let nj = Grib.Handle.get_int h "Nj" in
  let values = Grib.Handle.get_float_array h "values" in
  let values_ba = Grib.Handle.get_float_array_ba h "values" Bigarray.c_layout in
  let edition_string = Grib.Handle.get_as_string h "editionNumber" in
  let message = Grib.Handle.get_message_copy h in
  printf "grib version: %d\n" edition_number;
  printf "grib version (as string): %s\n" edition_string;
  printf "field name: %s\n" name;
  printf "average: %f\n" average;
  printf "NixNj: %dx%d\n" ni nj;
  printf "number of values: %d\n" (Array.length values);
  printf "number of values (ba): %d\n" (Bigarray.Array1.dim values_ba);
  Grib.Handle.delete h;

  (* Confirm that nothing changes after a message copy *)
  let h = Grib.Handle.of_message message in
  assert (edition_number = Grib.Handle.get_int h "editionNumber");
  assert (name = Grib.Handle.get_string h "name");
  assert (average = Grib.Handle.get_float h "average");
  assert (ni = Grib.Handle.get_int h "Ni");
  assert (nj = Grib.Handle.get_int h "Nj");
  assert (values = Grib.Handle.get_float_array h "values");
  assert
    (values_ba = Grib.Handle.get_float_array_ba h "values" Bigarray.c_layout);
  assert (message = Grib.Handle.get_message_copy h);
  Grib.Handle.delete h;
  ()
