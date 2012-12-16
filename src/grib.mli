module Message :
  sig
    (** The type of a GRIB message *)
    type t

    (** [of_string s] returns a message made up of the bytes in [s]. *)
    val of_string : string -> t

    (** [to_string t] returns a string containing the bytes from [t]. *)
    val to_string : t -> string

    (** [save ~mode m filename] saves the message [m] to [filename].
        [mode] specifies if [filename] should be created/overwritten or
        appended to. *)
    val save :
      ?perm:Batteries.File.permission ->
      mode:Batteries.File.open_out_flag list ->
      t -> string -> unit

    (** [save_list ~mode m filename] saves the messages [m] to [filename].
        [mode] specifies if [filename] should be created/overwritten or
        appended to. *)
    val save_list :
      ?perm:Batteries.File.permission ->
      mode:Batteries.File.open_out_flag list ->
      t list -> string -> unit

    (** [get_values m] is a shortcut command to retrieve the floating point
        values from the message [m]. *)
    val get_values : t -> float array
  end
module Inventory :
  sig
    type entry_t

    (** The type of a GRIB inventory entry from the [.idx] or [.inv] file as
        provided with GFS data and possibly others. *)
    type t = entry_t array

    (** Inventory entry getter functions *)
    val field : entry_t -> string
    val level : entry_t -> string
    val range : entry_t -> int * int option
    val date : entry_t -> string
    val kind : entry_t -> string
    val is_multi : entry_t -> bool

    (** [parse_blob lines] is like {!parse_lines} but it works on the entire
        inventory file at once. *)
    val parse_blob : string -> t

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
    val get_ranges :
      ?entries:(string * string) list ->
      t -> (int * int option) list
  end
module Handle :
  sig
    (** The type of a GRIB handle *)
    type t

    (** Potential value types for generic getters/setters *)
    type value_type_t =
      | TYPE_STRING
      | TYPE_LONG
      | TYPE_DOUBLE

    (** Container type for generic getters/setters *)
    type value_t =
      | String of string
      | Int of int
      | Float of float

    (** {3 Getting Data} *)

    (** All of the [get_*] functions raise [Invalid_argument] if the value(s)
        associated with the given key do not match the data requested. *)

    (** [get_size handle key] returns the number of elements associated with
        [key] in the GRIB [handle]. *)
    val get_size : t -> string -> int

    (** [get_native_type handle key] returns the native type of the value
        associated with [key]._*)
    val get_native_type : t -> string -> value_type_t

    (** [get_double_array handle key] returns the elements from [handle]
        associated with [key] as floating point values. *)
    val get_double_array : t -> string -> float array

    (** [get_string_any handle key] returns the elements from [handle]
        associated with [key] as a string, regardless of the native type
        of [key].*)
    val get_string_any : t -> string -> string

    (** [get_string handle key] returns the elements from [handle] associated
        with [key] as a string. *)
    val get_string : t -> string -> string

    (** [get_long handle key] returns the element from [handle] associated
        with [key] as an integer. *)
    val get_long : t -> string -> int

    (** [get_double handle key] returns the element from [handle] associated
        with [key] as a float. *)
    val get_double : t -> string -> float

    (** Alias for {!get_double_array}. *)
    val get_float_array : t -> string -> float array

    (** Alias for {!get_string_any}. *)
    val get_as_string : t -> string -> string

    (** Alias for {!get_long}. *)
    val get_int : t -> string -> int

    (** Alias for {!get_double}. *)
    val get_float : t -> string -> float

    (** [get_*_opt handle key] will return [Some v] if [key] has a value
        associated with it, or [None] otherwise. *)
    val get_float_array_opt : t -> string -> float array option
    val get_string_opt : t -> string -> string option
    val get_int_opt : t -> string -> int option
    val get_float_opt : t -> string -> float option
    val get_as_string_opt : t -> string -> string option

    (** Generic value getters which act like the getter functions above, but
        return {!value_t} rather than a native type.  *)
    val get : t -> string -> value_t
    val get_opt : t -> string -> value_t option

    (** [get_message_copy handle] returns the message associated with [handle]
        as a string of bytes. *)
    val get_message_copy : t -> Message.t

    (** {3 Setting Data} *)

    (** [set_long handle key x] sets [key] to [x] in [handle]. *)
    val set_long : t -> string -> int -> unit

    (** [set_double handle key x] sets [key] to [x] in [handle]. *)
    val set_double : t -> string -> float -> unit

    (** [set_string handle key x] sets [key] to [x] in [handle]. *)
    val set_string : t -> string -> string -> unit

    (** [set_double_array handle key x] sets [key] to [x] in [handle]. *)
    val set_double_array : t -> string -> float array -> unit

    (** [set_long_array handle key x] sets [key] to [x] in [handle]. *)
    val set_long_array : t -> string -> int array -> unit

    (** Alias for {!set_long} *)
    val set_int : t -> string -> int -> unit

    (** Alias for {!set_double} *)
    val set_float : t -> string -> float -> unit

    (** Alias for {!set_string} *)
    val set_string : t -> string -> string -> unit

    (** Alias for {!set_double_array} *)
    val set_float_array : t -> string -> float array -> unit

    (** Generic value setters which act like the setter fucntions above, but
        take a {!value_t} rather than a native type. *)
    val set : t -> string -> value_t -> unit

    (** {3 Iterators} *)

    (** [map_message f m] applies [f] to the {!Handle.t} associated with the
        message [m]. *)
    val map_message : (t -> 'a) -> Message.t -> 'a

    (** [apply_message f m] applies [f] to the {!Handle.t} associated with the
        message [m]. *)
    val apply_message : (t -> unit) -> Message.t -> unit

    (** [map_sample f sample] applies [f] to the handle based on [sample].
        [sample] refers to one of the samples which come with the GRIB API. *)
    val map_sample : (t -> 'a) -> string -> 'a

    (** [map_file f filename] applies [f] to each handle in [filename] and
        returns the results as a list. *)
    val map_file : (t -> 'a) -> string -> 'a list

    (** [iter_file f filename] applies [f] to each handle in [filename]. *)
    val iter_file : (t -> unit) -> string -> unit

    (** [filter_map_file f filename] applies [f] to each handle included in
        [filename], filtering the results as each handle is processed. *)
    val filter_map_file : (t -> 'a option) -> string -> 'a list

    (** {2 Module for iterating over GRIB message keys} *)
    module Keys : sig
      (** Flags to limit which keys we iterate over *)
      type iterator_flag_t =
        | ALL_KEYS
        | SKIP_READ_ONLY
        | SKIP_OPTIONAL
        | SKIP_EDITION_SPECIFIC
        | SKIP_CODED
        | SKIP_COMPUTED
        | SKIP_DUPLICATES
        | SKIP_FUNCTION

      (** [map f handle] returns a list of [f] applied to all keys in
          [handle]. *)
      val map :
        ?flags:iterator_flag_t list ->
        ?namespace:string ->
        (string -> 'a) -> t -> 'a list

      (** [iter f handle] applies [f] to each key in [handle]. *)
      val iter :
        ?flags:iterator_flag_t list ->
        ?namespace:string ->
        (string -> unit) -> t -> unit

      (** [filter_map f handle] returns a list of [f] applied to all keys
          in [handle].  If [f] returns [None] then that element will dropped
          from the final list. *)
      val filter_map :
        ?flags:iterator_flag_t list ->
        ?namespace:string ->
        (string -> 'a option) -> t -> 'a list
    end
  end
module Index :
  sig
    (** The type of a GRIB index *)
    type t

    (** An indexing key-value type *)
    type kv

    (** [*_key k v] build key-value pairs for defining an index. *)
    val double_key : string -> float -> kv
    val float_key : string -> float -> kv
    val long_key : string -> int -> kv
    val int_key : string -> int -> kv
    val string_key : string -> string -> kv

    (** [select index kv] selects the subset of values in [index] which
        match [kv]. *)
    val select : t -> kv -> unit

    (** [keys_of_kvs kvs] extracts the keys from the list of [kvs] to ease, for
        example, passing the [keys] argument to {!with_file_in}. *)
    val keys_of_kvs : kv list -> string list

    (** [of_file filename keys] returns an index on [filename] over [keys]. *)
    val of_file : string -> string list -> t

    (** [delete index] deletes [index] and frees the underlying resources. *)
    val delete : t -> unit

    (** [get index kvs f] will apply [f] to the handle in [index] which
        matches [kvs].  If no messages match, [None] is returned.  If
        multiple messages match, [Invalid_argument] is raised. *)
    val get : t -> kv list -> (Handle.t -> 'a) -> 'a option

    (** [with_file_in ?init filename keys f] will call [f] with the index from
        [filename], keyed on [keys]. [init] can be used to initialize the index
        to a specific set of [key, value] pairs. *)
    val with_file_in : ?init:kv list -> string -> string list -> (t -> 'a) -> 'a

    (** [map f index] applies [f] to each handle in [index] and
        returns the results as a list. *)
    val map : (Handle.t -> 'a) -> t -> 'a list

    (** [iter f index] applies [f] to each handle in [index]. *)
    val iter : (Handle.t -> unit) -> t -> unit

    (** [map_file f filename keys] is equivalent to calling
        [with_file_in filename keys (map f)] *)
    val map_file : (Handle.t -> 'a) -> string -> kv list -> 'a list

    (** [iter_file f filename keys] is equivalent to calling
        [with_file_in filename keys (iter f)] *)
    val iter_file : (Handle.t -> unit) -> string -> kv list -> unit
  end
module Iterator :
  sig
    (** The type of a GRIB iterator *)
    type t

    (** [next i] returns the next [(lat, lon, value)] from the iterator [i]. *)
    val next : t -> (float * float * float) option

    (** [previous i] returns the previous [(lat, lon, value)] from the
        iterator [i]. *)
    val previous : t -> (float * float * float) option

    (** [reset i] is undocumented in the GRIB API as of version 1.9.0.
        It hopefully resets the iterator to the first data point. *)
    val reset : t -> unit

    (** [iterator_in f h] applies [f] to an iterator associated with the
        {!Handle.t} [h]. *)
    val iterator_in : (t -> 'a) -> Handle.t -> 'a

    (** [iter f h] applies the function [f] to each value returned by an
        iterator on [h]. *)
    val iter : (float * float * float -> unit) -> Handle.t -> unit

    (** [map f h] applies the function [f] to each value returned by an
        iterator on [h] and returns the results as a list. *)
    val map : (float * float * float -> 'a) -> Handle.t -> 'a list

    (** [to_lat_lon_value h] returns a [(lats, lons, values)] tuple containing
        the values from the iterator associated with [h]. *)
    val to_lat_lon_value : Handle.t -> float list * float list * float list
  end
module Multi :
  sig
    (** Turn support on and off for multiple fields in a single message *)
    val support_on : unit -> unit
    val support_off : unit -> unit

    (** [messages_of_multi_message m] copies a messages for each field from the
        multi-field message [m] if multiple field support is on. *)
    val messages_of_multi_message : Message.t -> Message.t list
end
module Nearest :
  sig
    (** The type of a GRIB API nearest *)
    type t

    (** Data on a nearby point *)
    type near_t = {
      target : (float * float); (** The targeted point *)
      loc : (float * float); (** The location of a nearby point *)
      value : float; (** The value of the nearby point *)
      distance : float; (** Distance to the nearby point *)
      index : int; (** Index of the nearby point in the values array *)
    }

    (** Flags for {!find} *)
    type flag_t =
      | SAME_POINT (** Same point (location) from one check to the next *)
      | SAME_GRID (** Same grid from one check to the next *)
      | SAME_DATA (** Same data from one check to the next *)

    (** [of_handle h] creates a {!t} value from then handle [h]. *)
    val of_handle : Handle.t -> t

    (** [find ?flags t h p] finds the four nearest points to a given [p]
        ([lon, lat]) from the data in the handle [h] using the nearest [n]. *)
    val find :
      ?flags:flag_t list ->
      t -> Handle.t -> float * float -> near_t array

    (** [find_multiple ?mask h ps] finds the nearest point to each of a series
        of data points [ps].  If [mask] is [true] then [h] is considered to be
        a land-sea mask and the closest on-land point is used. *)
    val find_multiple :
      ?mask:bool ->
      Handle.t -> (float * float) array -> near_t array
end
module G2clib :
  sig
    (** NCEP g2clib bindings *)

    (** The type of a GRIB field *)
    type 'a field_t

    (** [of_message ?field message] returns a {!field_t} from [message] with
        unpacked field values and bitmap.

        @param field indicates which field in the message you want to extract.
        Defaults to [1] (the first field in the message).
    *)
    val of_message : ?field:int -> Message.t -> [ `unpacked ] field_t

    (** [of_handle ?field handle] returns a {!field_t} from [handle] with
        unpacked field values and bitmap.

        @param field indicates which field in the handle you want to extract.
        Defaults to [1] (the first field in the message).
    *)
    val of_handle : ?field:int -> Handle.t -> [ `unpacked ] field_t

    (** [get_values ?missing1 ?missing2 field] returns the unpacked data from
        [field], replacing missing values in complex packed grids if
        replacements are provided.

        @param missing1 will be substituted for any primary missing values if
        it is provided
        @param missing2 will be substituted for any seconary missing values if
        it is provided *)
    val get_values :
      ?missing1:float ->
      ?missing2:float ->
      [ `unpacked ] field_t -> float array

    (** [get_missing field] returns the value(s) associated with missing data
        in [field] if there are any.  If both values in the tuple are [None]
        then there is no special value for marking missing data.  If only
        the first value is [Some x] then [x] is the only value used to indicate
        missing data.  If the returned tuple is [Some a, Some b] then [a] is
        the primary missing value and [b] is the secondary missing value
        according to code table 5.5. *)
    val get_missing : 'a field_t -> float option * float option
  end

