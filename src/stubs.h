// Val_none, Val_some and Some_val are from:
// http://www.linux-nantes.org/~fmonnier/OCaml/ocaml-wrapping-c.php#ref_option
#define Val_none Val_int(0)

static inline value Val_some( value v ) {
    CAMLparam1( v );
    CAMLlocal1( some );
    some = caml_alloc(1, 0);
    Store_field( some, 0, v );
    CAMLreturn( some );
}

#define Some_val(v) Field(v,0)

// GRIB API

// Re-define the GRIB_CHECK macro to be more OCaml-friendly
#undef GRIB_CHECK
#define GRIB_CHECK(f, e) \
    if ((f) != (e)) { \
        const char *message; \
        message = grib_get_error_message( f ); \
        caml_invalid_argument( message ); \
    }

#define Handle_val(val) ((grib_handle *) val)
#define Index_val(val) ((grib_index *) val)
#define Iterator_val(val) ((grib_iterator *) val)
#define File_val(val) ((FILE *) val)
#define Keys_val(val) ((grib_keys_iterator *) val)
#define Nearest_val(val) (* ((grib_nearest **) Data_custom_val(val)))

// g2clib

#define Gribfield_val(val) (* ((gribfield **) Data_custom_val(val)))

