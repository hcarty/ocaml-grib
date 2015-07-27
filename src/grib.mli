module Message :
  sig
    type t
    (** The type of a GRIB message *)

    val of_bytes : bytes -> t
    (** [of_bytes b] returns a message made up of the bytes in [b]. *)

    val to_bytes : t -> bytes
    (** [to_bytes t] returns the {bytes} which make up [t]. *)

    val of_string : string -> t
    (** [of_string s] returns a message made up of the bytes in [s]. *)

    val to_string : t -> string
    (** [to_string t] returns a string containing the bytes from [t]. *)

    val of_bigarray :
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
      t
    (** [of_bigarray ba] returns a [t] containing the bytes from [ba]. *)

    val to_bigarray :
      t ->
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    (** [to_bigarray ba] returns a bigarray containing the bytes from [t]. *)

    val save : open_flag list -> int -> string -> t -> unit
    (** [save mode perm filename m] saves the message [m] to [filename].
        [mode] specifies if [filename] should be created/overwritten or
        appended to. *)

    val save_list : open_flag list -> int -> string -> t list -> unit
    (** [save_list mode perm filename m] saves the messages [m] to [filename].
        [mode] specifies if [filename] should be created/overwritten or
        appended to. *)
  end
module Handle :
  sig
    type t
    (** The type of a GRIB handle *)

    exception Invalid_handle
    (** Raised if a handle is used after being closed *)

    type value_type_t =
      | String_type
      | Int_type
      | Float_type
    (** Potential value types for generic getters/setters *)

    (** Container type for generic getters/setters *)
    type value_t =
      | String of string
      | Int of int
      | Float of float

    (** {3 Getting Data}

        All of the [get_*] functions raise [Invalid_argument] if the value(s)
        associated with the given key do not match the data requested. *)

    val get_size : t -> string -> int
    (** [get_size handle key] returns the number of elements associated with
        [key] in the GRIB [handle]. *)

    val get_native_type : t -> string -> value_type_t
    (** [get_native_type handle key] returns the native type of the value
        associated with [key]._*)

    val get_float_array : t -> string -> float array
    (** [get_float_array handle key] returns the elements from [handle]
        associated with [key] as floating point values. *)

    val get_float_array_ba :
      t -> string -> 'a Bigarray.layout ->
      (float, Bigarray.float64_elt, 'a) Bigarray.Array1.t
    (** [get_float_array_ba handle key] returns the elements from [handle]
        associated with [key] as floating point values. *)

    val get_float_array_into_ba :
      t -> string ->
      (float, Bigarray.float64_elt, 'a) Bigarray.Array1.t ->
      unit
    (** [get_float_array_into_ba handle key data] inserts the elements from
        [handle] associated with [key] as floating point values into [data].

        @raise Invalid_argument if [data] has a different number of elements
        than [key]. *)

    val get_string_any : t -> string -> string
    (** [get_string_any handle key] returns the elements from [handle]
        associated with [key] as a string, regardless of the native type
        of [key].*)

    val get_string : t -> string -> string
    (** [get_string handle key] returns the elements from [handle] associated
        with [key] as a string. *)

    val get_int : t -> string -> int
    (** [get_int handle key] returns the element from [handle] associated
        with [key] as an integer. *)

    val get_float : t -> string -> float
    (** [get_float handle key] returns the element from [handle] associated
        with [key] as a float. *)

    val get_as_string : t -> string -> string
    (** Alias for {!get_string_any}. *)

    val get_float_array_opt : t -> string -> float array option
    val get_string_opt : t -> string -> string option
    val get_int_opt : t -> string -> int option
    val get_float_opt : t -> string -> float option
    val get_as_string_opt : t -> string -> string option
    (** [get_*_opt handle key] will return [Some v] if [key] has a value
        associated with it, or [None] otherwise. *)

    val get : t -> string -> value_t
    val get_opt : t -> string -> value_t option
    (** Generic value getters which act like the getter functions above, but
        return {!value_t} rather than a native type.  *)

    val get_message_copy : t -> Message.t
    (** [get_message_copy handle] returns the message associated with [handle]
        as a string of bytes. *)

    (** {3 Setting Data} *)

    val set_int : t -> string -> int -> unit
    (** [set_int handle key x] sets [key] to [x] in [handle]. *)

    val set_float : t -> string -> float -> unit
    (** [set_float handle key x] sets [key] to [x] in [handle]. *)

    val set_string : t -> string -> string -> unit
    (** [set_string handle key x] sets [key] to [x] in [handle]. *)

    val set_float_array : t -> string -> float array -> unit
    (** [set_float_array handle key x] sets [key] to [x] in [handle]. *)

    val set_float_array_ba :
      t -> string ->
      (float, Bigarray.float64_elt, 'a) Bigarray.Array1.t ->
      unit
    (** [set_float_array_ba handle key x] sets [key] to [x] in [handle]. *)

    val set_int_array : t -> string -> int array -> unit
    (** [set_int_array handle key x] sets [key] to [x] in [handle]. *)

    val set : t -> string -> value_t -> unit
    (** Generic value setters which act like the setter fucntions above, but
        take a {!value_t} rather than a native type. *)

    val of_message : Message.t -> t
    (** [of_message m] returns a handle using the data in [m]. *)

    val of_sample : string -> t
    (** [of_sample name] returns a handle using the data from sample [name]. *)

    val delete : t -> unit
    (** [delete t] frees resources associated with [t]. *)

    (** {3 Iterators} *)

    val map_message : (t -> 'a) -> Message.t -> 'a
    (** [map_message f m] applies [f] to the {!Handle.t} associated with the
        message [m]. *)

    val map_sample : (t -> 'a) -> string -> 'a
    (** [map_sample f sample] applies [f] to the handle based on [sample].
        [sample] refers to one of the samples which come with the GRIB API. *)

    val fold_file : (t -> 'a -> 'a) -> string -> 'a -> 'a
    (** [fold_file f filename init] folds over the messages in [filename]. *)

    val map_file : (t -> 'a) -> string -> 'a list
    (** [map_file f filename] applies [f] to each handle in [filename] and
        returns the results as a list. *)

    val iter_file : (t -> unit) -> string -> unit
    (** [iter_file f filename] applies [f] to each handle in [filename]. *)

    val filter_map_file : (t -> 'a option) -> string -> 'a list
    (** [filter_map_file f filename] applies [f] to each handle included in
        [filename], filtering the results as each handle is processed. *)

    (** {2 Module for iterating over GRIB message keys} *)
    module Keys : sig
      type iterator_flag_t =
        | All_keys
        | Skip_read_only
        | Skip_optional
        | Skip_edition_specific
        | Skip_coded
        | Skip_computed
        | Skip_duplicates
        | Skip_function
      (** Flags to limit which keys we iterate over *)

      val map :
        ?flags:iterator_flag_t list ->
        ?namespace:string ->
        (string -> 'a) -> t -> 'a list
      (** [map f handle] returns a list of [f] applied to all keys in
          [handle]. *)

      val iter :
        ?flags:iterator_flag_t list ->
        ?namespace:string ->
        (string -> unit) -> t -> unit
      (** [iter f handle] applies [f] to each key in [handle]. *)

      val filter_map :
        ?flags:iterator_flag_t list ->
        ?namespace:string ->
        (string -> 'a option) -> t -> 'a list
      (** [filter_map f handle] returns a list of [f] applied to all keys
          in [handle].  If [f] returns [None] then that element will dropped
          from the final list. *)
    end
  end
module Index :
  sig
    type t
    (** The type of a GRIB index *)

    exception Invalid_index
    (** Raised if an index is used after being passed to {!delete}. *)

    type kv
    (** An indexing key-value type *)

    val float_key : string -> float -> kv
    val int_key : string -> int -> kv
    val string_key : string -> string -> kv
    (** [*_key k v] build key-value pairs for defining an index. *)

    val select_one : t -> kv -> unit
    (** [select_one index kv] selects the subset of values in [index] which
        match [kv]. *)

    val select : t -> kv list -> unit
    (** [select index kvs] selects the subset of values in [index] which
        match [kvs]. *)

    val select_float : t -> string -> float -> unit
    val select_int : t -> string -> int -> unit
    val select_string : t -> string -> string -> unit
    (** [select_* t k v] selects [k]ey and [v]alue on [t]. *)

    val keys_of_kvs : kv list -> string list
    (** [keys_of_kvs kvs] extracts the keys from the list of [kvs] to ease, for
        example, passing the [keys] argument to {!with_file_in}. *)

    val create : string list -> t
    (** [create keys] creates an empty index over [keys]. *)

    val add_file : t -> string -> unit
    (** [add_file index filename] will add the messages from [filename] to
        [index]. *)

    val of_file : string -> string list -> t
    (** [of_file filename keys] returns an index on [filename] over [keys]. *)

    val of_files : files:string list -> keys:string list -> t
    (** [of_files ~files ~keys] returns an index on [files] over [keys]. *)

    val delete : t -> unit
    (** [delete index] deletes [index] and frees the underlying resources.
        This will be called automatically if the index is garbage collected and
        the index is not already closed. *)

    val fold : (Handle.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold ?kvs f index init] folds [f] over each handle selected from [t],
        optionally initializing with [kvs]. *)

    val map : (Handle.t -> 'a) -> t -> 'a list
    (** [map f index] applies [f] to each handle in [index], optionally
        initializing with [kvs], and returns the results as a list. *)

    val iter : (Handle.t -> unit) -> t -> unit
    (** [iter f index] applies [f] to each handle in [index], optionally
        initializing with [kvs]. *)

    val with_file_in : ?init:kv list -> string -> string list -> (t -> 'a) -> 'a
    (** [with_file_in ?init filename keys f] will call [f] with the index from
        [filename], keyed on [keys]. [init] can be used to initialize the index
        to a specific set of [key, value] pairs. *)

    val with_files_in :
      ?init:kv list -> files:string list -> keys:string list -> (t -> 'a) -> 'a
    (** [with_files_in ?init ~files ~keys f] will call [f] with the index from
        [files], keyed on [keys]. [init] can be used to initialize the index
        to a specific set of [key, value] pairs. *)

    val write : t -> string -> unit
    (** [write index filename] writes [index] out to [filename]. *)

    val read : string -> t
    (** [read filename] reads [index] from [filename]. *)
  end
module Iterator :
  sig
    (** The type of a GRIB iterator *)
    type t

    val of_handle : Handle.t -> t
    (** [of_handle h] returns an iterator which will walk the data values in
        [h]. *)

    val delete : t -> unit
    (** [delete i] will free the resources associated with [i]. *)

    val next : t -> (float * float * float) option
    (** [next i] returns the next [(lat, lon, value)] from the iterator [i]. *)

    val previous : t -> (float * float * float) option
    (** [previous i] returns the previous [(lat, lon, value)] from the
        iterator [i]. *)

    val reset : t -> unit
    (** [reset i] is undocumented in the GRIB API as of version 1.9.0.
        It hopefully resets the iterator to the first data point. *)

    val iterator_in : (t -> 'a) -> Handle.t -> 'a
    (** [iterator_in f h] applies [f] to an iterator associated with the
        {!Handle.t} [h]. *)

    val iter : (float * float * float -> unit) -> Handle.t -> unit
    (** [iter f h] applies the function [f] to each value returned by an
        iterator on [h]. *)

    val map : (float * float * float -> 'a) -> Handle.t -> 'a list
    (** [map f h] applies the function [f] to each value returned by an
        iterator on [h] and returns the results as a list. *)

    val to_lat_lon_value : Handle.t -> float list * float list * float list
    (** [to_lat_lon_value h] returns a [(lats, lons, values)] tuple containing
        the values from the iterator associated with [h]. *)
  end
module Multi :
  sig
    val support_on : unit -> unit
    val support_off : unit -> unit
    (** Turn support on and off for multiple fields in a single message *)

    val messages_of_multi_message : Message.t -> Message.t list
    (** [messages_of_multi_message m] copies a messages for each field from the
        multi-field message [m] if multiple field support is on. *)
end
module Nearest :
  sig
    type t
    (** The type of a GRIB API nearest *)

    type near_t = {
      target : (float * float); (** The targeted point *)
      loc : (float * float); (** The location of a nearby point *)
      value : float; (** The value of the nearby point *)
      distance : float; (** Distance to the nearby point *)
      index : int; (** Index of the nearby point in the values array *)
    }
    (** Data on a nearby point *)

    type flag_t =
      | Same_point (** Same point (location) from one check to the next *)
      | Same_grid (** Same grid from one check to the next *)
      | Same_data (** Same data from one check to the next *)
    (** Flags for {!find} *)

    val of_handle : Handle.t -> t
    (** [of_handle h] creates a {!t} value from then handle [h]. *)

    val find :
      ?flags:flag_t list ->
      t -> Handle.t -> float * float -> near_t array
    (** [find ?flags t h p] finds the four nearest points to a given [p]
        ([lon, lat]) from the data in the handle [h] using the nearest [n]. *)

    val find_multiple :
      ?mask:bool ->
      Handle.t -> (float * float) array -> near_t array
    (** [find_multiple ?mask h ps] finds the nearest point to each of a series
        of data points [ps].  If [mask] is [true] then [h] is considered to be
        a land-sea mask and the closest on-land point is used. *)
end
module G2clib :
  sig
    (** {2 NCEP g2clib bindings} *)

    type 'a field
    (** The type of a GRIB field *)

    val of_message : ?field:int -> Message.t -> [ `unpacked ] field
    (** [of_message ?field message] returns a {!field} from [message] with
        unpacked field values and bitmap.

        @param field indicates which field in the message you want to extract.
        Defaults to [1] (the first field in the message).
    *)

    val of_handle : ?field:int -> Handle.t -> [ `unpacked ] field
    (** [of_handle ?field handle] returns a {!field} from [handle] with
        unpacked field values and bitmap.

        @param field indicates which field in the handle you want to extract.
        Defaults to [1] (the first field in the message).
    *)

    val get_float_array :
      ?missing1:float ->
      ?missing2:float ->
      [ `unpacked ] field -> float array
    (** [get_float_array ?missing1 ?missing2 field] returns the unpacked data
        from [field], replacing missing values in complex packed grids if
        replacements are provided.

        @param missing1 will be substituted for any primary missing values if
        it is provided
        @param missing2 will be substituted for any seconary missing values if
        it is provided *)

    val get_float_array_ba :
      ?missing1:float ->
      ?missing2:float ->
      [ `unpacked ] field -> 'a Bigarray.layout ->
      (float, Bigarray.float64_elt, 'a) Bigarray.Array1.t
    (** Like {!get_float_array} except that the result is a bigarray. *)

    val get_float_array_into_ba :
      ?missing1:float ->
      ?missing2:float ->
      [ `unpacked ] field ->
      (float, Bigarray.float64_elt, 'a) Bigarray.Array1.t -> unit
    (** Like {!get_float_array_ba} except that the data are written into the
        given bigarray.

        @raise Invalid_argument if the given bigarray has a different number of
        elemetns than the given {!field}. *)

    val get_missing : 'a field -> float option * float option
    (** [get_missing field] returns the value(s) associated with missing data
        in [field] if there are any.  If both values in the tuple are [None]
        then there is no special value for marking missing data.  If only
        the first value is [Some x] then [x] is the only value used to indicate
        missing data.  If the returned tuple is [Some a, Some b] then [a] is
        the primary missing value and [b] is the secondary missing value
        according to code table 5.5. *)
  end

