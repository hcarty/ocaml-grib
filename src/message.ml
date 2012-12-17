(** Module for working with GRIB messages *)

open Batteries

(** The raw GRIB data, able to be processed with the {!Grib} module or written
    directly to disk as a fresh GRIB file. *)
type t = string

(** Conversion to/from byte strings *)
external of_string : string -> t = "%identity"
external to_string : t -> string = "%identity"

(** [save_list ?perm ~mode m filename] saves the GRIB messages [m] to
    [filename].  Use [mode] to specify if the file should be appended to (add
    new GRIB messages to an existing GRIB file) or if the file should be
    created/overwritten.  Use [perm] to specify the desired file
    permissions (defaults to current umask settings). *)
let save_list ?perm ~mode m filename =
  (* Default to using the user's umask permissions *)
  let perm =
    match perm with
    | None -> File.unix_perm 0o666
    | Some p -> p
  in
  match m with
  | [] ->
      (* No messages, nothing to do *)
      ()
  | l ->
      (* Write each message to the file, in order *)
      File.with_file_out ~perm ~mode filename (
        fun fout ->
          List.iter (String.print fout) l
      )

(** [save ?perm ~mode m filename] acts like {!save_list}, acting on one
    message rather than a list. *)
let save ?perm ~mode m filename =
  save_list ?perm ~mode [m] filename

