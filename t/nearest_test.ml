open Grib
open OUnit2

let nearest _ctx =
  let h = Handle.of_sample "GRIB2" in
  begin
    let n = Nearest.of_handle h in
    match Nearest.find n h (0.0, 0.0) with
    | [|_one; _two; _three; _four|] -> ()
    | _ -> assert_string "wrong number of array elements"
  end;
  Gc.compact ();
  begin
    match Nearest.find_multiple h [|0.0, 0.0|] with
    | [|_one|] -> ()
    | _ -> assert_string "wrong number of array elements"
  end;
  Handle.delete h;
  ()

let t =
  "Nearest" >::: [
    "main" >:: nearest
  ]
