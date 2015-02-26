open Grib

let () =
  let message = Handle.map_sample Handle.get_message_copy "GRIB2" in
  (* Save one message *)
  let outfile = Filename.temp_file "message" "test" in
  Message.save ~mode:[`create] message outfile;
  Sys.remove outfile;
  (* Save multiple messages at once *)
  let outfile = Filename.temp_file "message" "test" in
  Message.save_list ~mode:[`create] [message; message] outfile;
  Sys.remove outfile
