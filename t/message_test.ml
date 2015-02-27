open Grib

let () =
  Multi.support_on ();
  Multi.support_off ();
  Multi.support_on ();
  let message = Handle.map_sample Handle.get_message_copy "GRIB2" in
  begin
    match Multi.messages_of_multi_message message with
    | [m] -> assert (m = message)
    | _ -> assert false
  end;
  (* Save one message *)
  let outfile = Filename.temp_file "message" "test" in
  Message.save ~mode:[`create] message outfile;
  Sys.remove outfile;
  (* Save multiple messages at once *)
  let outfile = Filename.temp_file "message" "test" in
  Message.save_list ~mode:[`create] [message; message] outfile;
  Sys.remove outfile
