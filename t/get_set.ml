open OUnit2

let get _ctx =
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
  Grib.Handle.delete h;

  (* Confirm that nothing changes after a message copy *)
  let h = Grib.Handle.of_message message in
  assert_bool "editionNumber"
    (edition_number = Grib.Handle.get_int h "editionNumber");
  assert_bool "editionNumber (as string)"
    (edition_string = Grib.Handle.get_as_string h "editionNumber");
  assert_bool "name" (name = Grib.Handle.get_string h "name");
  assert_bool "average" (average = Grib.Handle.get_float h "average");
  assert_bool "Ni" (ni = Grib.Handle.get_int h "Ni");
  assert_bool "Nj" (nj = Grib.Handle.get_int h "Nj");
  assert_bool "values" (values = Grib.Handle.get_float_array h "values");
  assert_bool "values ba"
    (values_ba = Grib.Handle.get_float_array_ba h "values" Bigarray.c_layout);
  assert_bool "message" (message = Grib.Handle.get_message_copy h);
  Grib.Handle.delete h;
  ()

let t =
  "get_set" >::: [
    "general" >:: get
  ]
