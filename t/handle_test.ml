open Grib
open OUnit2

let handle _ctx =
  let h = Handle.of_sample "GRIB2" in
  assert_bool "get_size" (Handle.get_size h "name" = 1);
  assert_bool "get_native_type"
    (Handle.get_native_type h "name" = Handle.String_type);

  ignore (Handle.Keys.map (fun x -> x) h);
  Handle.Keys.iter ignore h;
  assert_bool "Keys.filter_map" (Handle.Keys.filter_map (fun x -> None) h = []);

  (* Multiple deletions should be silent no-ops *)
  Handle.delete h;
  Handle.delete h;
  begin
    try
      ignore (Handle.get_string h "name");
      assert false
    with
    | Handle.Invalid_handle -> ()
  end;
  ()

let t =
  "Handle" >::: [
    "main" >:: handle
  ]
