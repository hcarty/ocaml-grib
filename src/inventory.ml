(** Parsing GRIB idx files *)

open Printf

(** Inventory message types *)
type message_kind_t =
  | Single of string
  | Multi of string

(** An entry in an idx file. *)
type entry_t = {
  message_kind : message_kind_t;
  first_byte : int;
  last_byte : int option;
  date : string;
  field : string;
  level : string;
  time : string;
  kind : string;
}

(** A list of inventory entries *)
type t = entry_t array

(* Potential simplified version using the Csv library
let of_string s =
  let a =
    Csv.of_string ~separator:':' s
    |> Csv.input_all
    |> Csv.to_array
  in
  Array.mapi (
    fun i line ->
      let message_kind =
        let index = line.(0) in
        if String.contains line.(0) '.' then Multi index
        else Single index
      in
      let last_byte =
        if i < Array.length a - 1 then
          Some (int_of_string a.(i + 1).(1) - 1)
        else
          None
      in
      {
        message_kind;
        first_byte = int_of_string line.(1);
        last_byte;
        date = line.(2);
        field = line.(3);
        level = line.(4);
        time = line.(5);
        kind = line.(6);
      }
  ) a
*)

(** Inventory entry getter functions *)
let field e = e.field
let level e = e.level
let range e = e.first_byte, e.last_byte
let date e = e.date
let kind e = e.kind
let is_multi e =
  match e.message_kind with
  | Single _ -> false
  | Multi _ -> true

(** [find_next_message_index inv i] finds the next inventory entry in [inv]
    after [i] which has a different entry value.  This will be the next
    independent GRIB message.
    Returns [Some n] if there is a next entry, and [None] if there is not. *)
let find_next_message_index inv i =
  (* The base message we are checking against *)
  let base = inv.(i) in

  (* The index of the last inventory entry *)
  let last_i = Array.length inv - 1 in

  (* Find the next independent message *)
  let next_i = ref (i + 1) in
  let continue = ref true in
  while !continue do
    if !next_i > last_i then (
      continue := false;
    )
    else (
      if inv.(!next_i).first_byte = base.first_byte then (
        (* Still on the same entry - move along *)
        incr next_i;
      )
      else (
        (* This is a new entry, so we're done! *)
        continue := false;
      )
    )
  done;
  if !next_i <= last_i then (
    (* We found the next entry *)
    Some !next_i
  )
  else (
    (* There is no next entry - [i] is the last one *)
    None
  )

(** [parse_lines lines] will return an array of {!t} values containing the
    GRIB field definitions given in [lines]. *)
let parse_lines lines =
  (* First pass... *)
  let indices =
    Array.filter_map (
      fun line ->
        let pieces = Pcre.asplit ~pat:":" line in
        let len = Array.length pieces in
        match len with
        | 6
        | 7
        | 8
        | 9 ->
            (* A recognized line with 6, 7, 8 or 9 ":" delimited pieces *)
            (* Is this a single- or multi-field entry? *)
            let entry_id_pieces = Pcre.asplit ~pat:"\\." pieces.(0) in
            let message_kind =
              match Array.length entry_id_pieces with
              | 1 -> Single entry_id_pieces.(0)
              | 2 -> Multi entry_id_pieces.(0)
              | n ->
                  eprintf "Found strange field index, assuming single: %s"
                    pieces.(0);
                  Single pieces.(0)
            in
            (* 8-piece line have a byte range in the second field, but we're
               ignoring that. *)
            let index_offset = if len = 8 then 1 else 0 in
            (* Build the base entry *)
            Some {
              message_kind;
              first_byte = int_of_string pieces.(1);
              last_byte = None;
              date = pieces.(2 + index_offset);
              field = pieces.(3 + index_offset);
              level = pieces.(4 + index_offset);
              kind = pieces.(5 + index_offset);
            }
        | _ ->
            (* Skip/ignore malformed lines *)
            None
    ) lines
  in

  (* Second pass to update the byte ranges *)
  let n_indices = Array.length indices in
  let update_last_byte i entry =
    match i with
    | n when n = n_indices - 1 ->
        (* For the last entry, the last byte is the last byte in the file. *)
        entry
    | _ ->
        (* The last byte of this entry is one before the first byte of the
           next full entry, if there is a next full entry.
           The only time we should NOT find a next full entry is when the
           last entry in the file is a multi-field message. *)
        let next_i_maybe = find_next_message_index indices i in
        Option.map_default (
          fun next_i ->
            let last_byte = Some (indices.(next_i).first_byte - 1) in
            { entry with last_byte }
        ) entry next_i_maybe
  in
  Array.mapi update_last_byte indices

(** [parse_blob s] is like {!parse_lines}, except that it takes the entire
    index file as a single string and splits it in to lines itself. *)
let parse_blob s =
  let lines = Pcre.asplit ~pat:"\n" s in
  parse_lines lines

(** [to_hashtbl key_f index] converts the array of index entries in [index]
    to a hash table for simpler field access.  [key_f] is used to generate
    the key which matches a given index entry. *)
let to_hashtbl key_f index =
  let hash = Hashtbl.create (Array.length index) in
  Array.iter (
    fun field -> Hashtbl.add hash (key_f field) field
  ) index;
  hash

(** [to_range_hashtbl inventory] is like [to_hashtbl] but instead of a
    [(field_name, field_height_level)] key matching a {!t} it instead
    matches the byte range for that field.  This is provided primarily to
    simplify the process of downloading individual GRIB fields. *)
let to_range_hashtbl inventory =
  let hash = Hashtbl.create (Array.length inventory) in
  Array.iter (
    fun field ->
      Hashtbl.add hash
        (field.field, field.level) (field.first_byte, field.last_byte)
  ) inventory;
  hash

(* A function to extract all of the entries from an inventory *)
let all_entries inventory =
  Array.map (fun entry -> entry.field, entry.level) inventory
  |> Array.to_list

(** [get_ranges ?entries inventory] will return a list of
    [(first_byte, last_byte)] values which cover the data requested by the
    [(field, level)] pairs in [entries].  If [entries] is not provided
    then ranges for all entries in [inventory] are returned.

    Multi-field GRIB message entries will be filtered appropriately so that
    each message is only downloaded once.
    
    N.B. This will not work as expected if multiple messages with the same
    [(field, level)] names exist in [inventory].
    
    Raises [Not_found] if any values in [entries] are
    not found in [inventory]. *)
let get_ranges ?entries (inventory : t) =
  (* If entries isn't provided, default to everything *)
  let entries = Option.default (all_entries inventory) entries in

  (* All ranges, and the ranges we have added so far *)
  let range_hash = to_range_hashtbl inventory in
  let added_ranges = Hashtbl.create 0 in

  let ranges =
    List.map (
      fun entry_key ->
        (* Catch entries with duplicate names *)
        let ranges = Hashtbl.find_all range_hash entry_key in
        List.filter_map (
          fun range ->
            if Hashtbl.mem added_ranges range then (
              (* We've already added this range, so there is no need to do so
                 again - this [None] will be filtered out completely. *)
              None
            )
            else (
              (* Record that we've added this range already *)
              Hashtbl.add added_ranges range true;
              (* A new range - leaves [range] after filter_map is done. *)
              Some range
            )
        ) ranges
    ) entries
  in
  (* Flatten the list of lists *)
  List.concat ranges

