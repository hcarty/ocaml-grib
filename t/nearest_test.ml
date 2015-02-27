open Grib

let () =
  let h = Handle.of_sample "GRIB2" in
  begin
    let n = Nearest.of_handle h in
    match Nearest.find n h (0.0, 0.0) with
    | [|_one; _two; _three; _four|] -> ()
    | _ -> assert false
  end;
  Gc.compact ();
  begin
    match Nearest.find_multiple h [|0.0, 0.0|] with
    | [|_one|] -> ()
    | _ -> assert false
  end;
  Handle.delete h;
  ()
