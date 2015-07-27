/*
 * An OCaml wrapper for NCEP's NCO library
 * Hezekiah M. Carty
 *
 */

/* The "usual" OCaml includes */
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/custom.h>

/* GRIB API include */
#include <grib_api.h>

/* g2clib API include */
#include "g2clib-src/grib2.h"

#include "stubs.h"

/* For debugging - we want to have access to printf, stderr and such */
#include <stdio.h>
#include <string.h>

// Allow for ridiculously long exception strings.
#define MAX_EXCEPTION_MESSAGE_LENGTH 10000

//
//
// GRIB field handling
//
//

// OCaml handler to free a GRIB field when it is GC'd
void finalize_gribfield( value ml_field ) {
    gribfield *field;
    field = Gribfield_val( ml_field );
    g2_free( field );
    return;
}

// Definition for custom OCaml handler functions
static struct custom_operations gribfield_custom_ops = {
    identifier: "grib field handling",
    finalize: finalize_gribfield,
    compare: custom_compare_default,
    hash: custom_hash_default,
    serialize: custom_serialize_default,
    deserialize: custom_deserialize_default
};

value Val_gribfield( gribfield *field ) {
    gribfield **store;
    value ret;
    ret = caml_alloc_custom(&gribfield_custom_ops, sizeof(store), 0, 1);
    store = Data_custom_val(ret);
    *store = field;
    return ret;
}

//
//
// Unpacking
//
//

// Get GRIB field from a message
value ml_g2_getfld( value message, value field_num, value unpack, value expand ) {
    CAMLparam4( message, field_num, unpack, expand );

    int result;
    gribfield *field;

    result = g2_getfld( (unsigned char *)String_val( message ), Int_val( field_num ), Bool_val( unpack ), Bool_val( expand ), &field );

    // Raise an exception if we fail to extract a field
    if ( result != 0 ) {
        caml_invalid_argument( "Bad message" );
    }

    CAMLreturn( Val_gribfield( field ) );
}

// Get GRIB field from a GRIB API handle
value ml_g2_getfld_handle( value handle, value field_num, value unpack, value expand ) {
    CAMLparam4( handle, field_num, unpack, expand );

    int result;
    gribfield *field;
    const void *message;
    size_t size;

    // Get a pointer to the message embedded in the handle.  No copying is
    // required because the message isn't being passed back to OCaml.
    GRIB_CHECK( grib_get_message( Handle_val( handle ), (const void **)&message, &size ), 0 );

    result = g2_getfld( (unsigned char *)message, Int_val( field_num ), Bool_val( unpack ), Bool_val( expand ), &field );

    // Raise an exception if we fail to extract a field
    if ( result != 0 ) {
        caml_invalid_argument( "Bad message from handle" );
    }

    CAMLreturn( Val_gribfield( field ) );
}

value ml_get_ngrdpts( value ml_field ) {
    CAMLparam1( ml_field );
    gribfield *field;
    field = Gribfield_val( ml_field );
    CAMLreturn( Val_int( field->ngrdpts ) );
}

#define ML_G2_UNDEF 1.111e20

// Get grid values from a GRIB field
value ml_get_data( value ml_substitute1, value ml_substitute2, value ml_field ) {
    CAMLparam3( ml_substitute1, ml_substitute2, ml_field );
    CAMLlocal1( ml_data );

    int i;
    double v;
    float missing[2];
    int num_missing;

    double substitute1 = ML_G2_UNDEF;
    double substitute2 = ML_G2_UNDEF;

    gribfield *field;

    field = Gribfield_val( ml_field );
    g2_miss( field, missing, &num_missing );

    // Check to see if we are going to fill in any missing values
    if ( num_missing >= 1 && ml_substitute1 != Val_none ) {
        substitute1 = Double_val( Some_val( ml_substitute1 ) );
    }

    // Check to see if there are two possible missing values
    if ( num_missing >= 2 && ml_substitute2 != Val_none ) {
        substitute2 = Double_val( Some_val( ml_substitute2 ) );
    }

    // Allocate an OCaml array and copy the data over
    ml_data = caml_alloc( field->ngrdpts * Double_wosize, Double_array_tag );
    for ( i = 0; i < field->ngrdpts; i++ ) {
        v = field->fld[i];
        if ( num_missing >= 1 && substitute1 != ML_G2_UNDEF && v == missing[0] ) {
            v = substitute1;
        }
        else if ( num_missing >= 2 && substitute2 != ML_G2_UNDEF && v == missing[1] ) {
            v = substitute2;
        }
        Store_double_field( ml_data, i, v );
    }

    // Return the OCaml-formatted data copy
    CAMLreturn( ml_data );
}

// Get grid values from a GRIB field
value ml_get_data_ba( value ml_substitute1, value ml_substitute2, value ml_field, value ba ) {
    CAMLparam4( ml_substitute1, ml_substitute2, ml_field, ba );

    int i;
    double v;
    float missing[2];
    int num_missing;

    double substitute1 = ML_G2_UNDEF;
    double substitute2 = ML_G2_UNDEF;

    gribfield *field;

    double *data;

    data = (double *)Caml_ba_data_val( ba );

    field = Gribfield_val( ml_field );
    g2_miss( field, missing, &num_missing );

    if ( Caml_ba_array_val(ba)->dim[0] != field->ngrdpts ) {
        caml_invalid_argument( "Wrong number of elements in bigarray" );
    }

    // Check to see if we are going to fill in any missing values
    if ( num_missing >= 1 && ml_substitute1 != Val_none ) {
        substitute1 = Double_val( Some_val( ml_substitute1 ) );
    }

    // Check to see if there are two possible missing values
    if ( num_missing >= 2 && ml_substitute2 != Val_none ) {
        substitute2 = Double_val( Some_val( ml_substitute2 ) );
    }

    // Allocate an OCaml array and copy the data over
    for ( i = 0; i < field->ngrdpts; i++ ) {
        v = field->fld[i];
        if ( num_missing >= 1 && substitute1 != ML_G2_UNDEF && v == missing[0] ) {
            v = substitute1;
        }
        else if ( num_missing >= 2 && substitute2 != ML_G2_UNDEF && v == missing[1] ) {
            v = substitute2;
        }
        data[i] = v;
    }

    CAMLreturn( Val_unit );
}

// Get missing values, if there are any
value ml_g2_miss( value ml_field ) {
    CAMLparam1( ml_field );
    CAMLlocal1( ml_missing );

    float missing[2];
    int num_missing;

    g2_miss( Gribfield_val( ml_field ), missing, &num_missing );

    // Allocate an OCaml tuple to send the values back
    ml_missing = caml_alloc( 2, 0 );
    Store_field(
        ml_missing, 0,
            num_missing >= 1 ? Val_some( caml_copy_double( missing[0] ) ) : Val_none );
    Store_field(
        ml_missing, 1,
            num_missing >= 2 ? Val_some( caml_copy_double( missing[1] ) ) : Val_none );

    // Return the tuple
    CAMLreturn( ml_missing );
}
