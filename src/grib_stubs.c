/*
 * An OCaml wrapper for the ECMWF GRIB C library.
 * by Hezekiah M. Carty
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

#include "stubs.h"

/* For debugging - we want to have access to printf, stderr and such */
#include <stdio.h>
#include <string.h>

// Allow for ridiculously long exception strings.
#define MAX_EXCEPTION_MESSAGE_LENGTH 10000

// The GRIB API sometimes returns very short lengths for strings
#define MIN_STRING_LENGTH 256

//
//
// Working with GRIB handles
//
//

// Free a grib handle
value ml_grib_handle_delete( value handle ) {
    CAMLparam1( handle );

    GRIB_CHECK( grib_handle_delete( Handle_val( handle ) ), 0 );

    CAMLreturn( Val_unit );
}

// Find out how many values are associated with a key in a handle
value ml_grib_get_size( value handle, value key ) {
    CAMLparam2( handle, key );

    size_t size;

    GRIB_CHECK( grib_get_size( Handle_val( handle ), String_val( key ), &size ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( Val_int( size ) );
}

// Retrieve data in double-precisions floating point format
value ml_grib_get_double_array( value handle, value key ) {
    CAMLparam2( handle, key );
    CAMLlocal1( data );

    size_t in_size, out_size;
    int type;

    GRIB_CHECK( grib_get_native_type( Handle_val( handle ), String_val( key ), &type ), 0 );

    if ( type != GRIB_TYPE_DOUBLE ) {
        caml_invalid_argument( "Incorrect data type" );
    }

    in_size = Int_val( ml_grib_get_size( handle, key ) );
    out_size = in_size;

    // Allocate an array to hold the output data
    data = caml_alloc( in_size * Double_wosize, Double_array_tag );

    GRIB_CHECK( grib_get_double_array( Handle_val( handle ), String_val( key ), (double *) data, &out_size ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( data );
}

value ml_grib_get_double_array_ba( value handle, value key, value data ) {
    CAMLparam2( handle, key );

    size_t in_size, out_size;
    int type;

    GRIB_CHECK( grib_get_native_type( Handle_val( handle ), String_val( key ), &type ), 0 );

    if ( type != GRIB_TYPE_DOUBLE ) {
        caml_invalid_argument( "Incorrect data type" );
    }

    in_size = Int_val( ml_grib_get_size( handle, key ) );
    out_size = in_size;

    GRIB_CHECK( grib_get_double_array( Handle_val( handle ), String_val( key ), (double *)Caml_ba_data_val( data ), &out_size ), 0 );

    CAMLreturn( Val_unit );
}

int ml_native_type_of_int( int c_type ) {
    switch ( c_type ) {
        case GRIB_TYPE_STRING:
            return 0;
            break;
        case GRIB_TYPE_LONG:
            return 1;
            break;
        case GRIB_TYPE_DOUBLE:
            return 2;
            break;
        default:
            caml_invalid_argument( "Unhandled GRIB value type" );
            return -1;
    }
}

// Get the native type of the value associated with the given key
value ml_grib_get_native_type( value handle, value key ) {
    CAMLparam2( handle, key );

    int c_type;

    GRIB_CHECK( grib_get_native_type( Handle_val( handle ), String_val( key ), &c_type ), 0 );

    CAMLreturn( Val_int( ml_native_type_of_int( c_type ) ) );
}

// Retrieve data in string format, regardless of the native type
value ml_grib_get_string_any( value handle, value key ) {
    CAMLparam2( handle, key );

    size_t in_size, out_size;

    in_size = Int_val( ml_grib_get_size( handle, key ) );
    if (in_size < MIN_STRING_LENGTH) {
        // If the string length is too short, then allocate extra space for it
        in_size = MIN_STRING_LENGTH;
    }
    out_size = in_size;

    // Allocate an array to hold the output data
    char data[in_size];

    GRIB_CHECK( grib_get_string( Handle_val( handle ), String_val( key ), data, &out_size ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( caml_copy_string( data ) );
}

// Retrieve data in string format
value ml_grib_get_string( value handle, value key ) {
    CAMLparam2( handle, key );

    int type;

    GRIB_CHECK( grib_get_native_type( Handle_val( handle ), String_val( key ), &type ), 0 );

    if ( type != GRIB_TYPE_STRING ) {
        caml_invalid_argument( "Incorrect data type" );
    }

    CAMLreturn( ml_grib_get_string_any( handle, key ) );
}

// Retrieve data in long (int) format
value ml_grib_get_long( value handle, value key ) {
    CAMLparam2( handle, key );

    int type;

    GRIB_CHECK( grib_get_native_type( Handle_val( handle ), String_val( key ), &type ), 0 );

    if ( type != GRIB_TYPE_LONG ) {
        caml_invalid_argument( "Incorrect data type" );
    }

    long datum;

    GRIB_CHECK( grib_get_long( Handle_val( handle ), String_val( key ), &datum ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( Val_int( datum ) );
}

// Retrieve data in double format
value ml_grib_get_double( value handle, value key ) {
    CAMLparam2( handle, key );

    int type;

    GRIB_CHECK( grib_get_native_type( Handle_val( handle ), String_val( key ), &type ), 0 );

    if ( type != GRIB_TYPE_DOUBLE ) {
        caml_invalid_argument( "Incorrect data type" );
    }

    double datum;

    GRIB_CHECK( grib_get_double( Handle_val( handle ), String_val( key ), &datum ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( caml_copy_double( datum ) );
}

// Set data in long format
value ml_grib_set_long( value handle, value key, value val ) {
    CAMLparam3( handle, key, val );

    GRIB_CHECK( grib_set_long( Handle_val( handle ), String_val( key ), Int_val( val ) ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( Val_unit );
}

// Set data in double format
value ml_grib_set_double( value handle, value key, value val ) {
    CAMLparam3( handle, key, val );

    GRIB_CHECK( grib_set_double( Handle_val( handle ), String_val( key ), Double_val( val ) ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( Val_unit );
}

// Set data in string format
value ml_grib_set_string( value handle, value key, value val ) {
    CAMLparam3( handle, key, val );

    size_t length;

    length = caml_string_length( val );

    GRIB_CHECK( grib_set_string( Handle_val( handle ), String_val( key ), String_val( val ), &length ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( Val_unit );
}

// Set data in double array format
value ml_grib_set_double_array( value handle, value key, value val ) {
    CAMLparam3( handle, key, val );

    size_t length;

    length = Wosize_val( val ) / Double_wosize;

    // OCaml float arrays can be cast directly as double arrays
    GRIB_CHECK( grib_set_double_array( Handle_val( handle ), String_val( key ), (double *)val, length ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( Val_unit );
}

// Set data in double array (bigarray) format
value ml_grib_set_double_array_ba( value handle, value key, value val ) {
    CAMLparam3( handle, key, val );

    size_t length;

    length = Caml_ba_array_val( val )->dim[0];

    GRIB_CHECK( grib_set_double_array( Handle_val( handle ), String_val( key ), Caml_ba_data_val( val ), length), 0 );

    CAMLreturn( Val_unit );
}

// Set data in long array format
value ml_grib_set_long_array( value handle, value key, value val ) {
    CAMLparam3( handle, key, val );

    size_t length;

    length = Wosize_val( val );

    // Copy the array
    long val_copy[ length ];
    int i;
    for ( i = 0; i < length; i++ ) {
        val_copy[ i ] = Field( val, i );
    }

    GRIB_CHECK( grib_set_long_array( Handle_val( handle ), String_val( key ), val_copy, length ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( Val_unit );
}

// Retrieve the message (raw bytes) associated with a handle
value ml_grib_get_message_copy( value handle ) {
    CAMLparam1( handle );
    CAMLlocal1( ml_message );

    const void *message;
    size_t size;

    grib_handle *h;
    h = Handle_val( handle );

    // Get a pointer to the buffer, and more importantly for us the size in
    // bytes.
    GRIB_CHECK( grib_get_message( h, (const void **)&message, &size ), 0 );

    // Allocate an OCaml string and copy the message over to that
    ml_message = caml_alloc_string( size );
    memcpy( String_val( ml_message ), message, size );

    CAMLreturn( ml_message );
}

// Create a handle from a message, copying the message to avoid GC issues
value ml_grib_handle_new_from_message_clone( value data ) {
    CAMLparam1( data );

    // Get the message length
    size_t length;
    length = caml_string_length( data );

    // Treat the given string like a raw blob of bytes
    void *data_ptr;
    data_ptr = (void *) String_val( data );

    // A handle with a copy of the OCaml message, so there is no GC
    // tie between this handle and the original message.
    grib_handle *handle;

    handle = grib_handle_new_from_message_copy( NULL, data_ptr, length );

    // If the message is invalid, raise Invalid_argument
    if ( handle == NULL ) {
        caml_invalid_argument( "Bad message" );
    }

    CAMLreturn( (value)handle );
}

// Create a handle from a GRIB API sample
value ml_grib_handle_new_from_samples( value res_name ) {
    CAMLparam1( res_name );

    grib_handle *handle;
    handle = grib_handle_new_from_samples( NULL, String_val( res_name ) );

    // If the resource is invalid, raise Invalid_argument
    if ( handle == NULL ) {
        caml_invalid_argument( "Bad/missing sample" );
    }

    CAMLreturn( (value)handle );
}

// Open a file handle, GRIB-style
value ml_grib_open_file( value filename ) {
    CAMLparam1( filename );

    FILE *fin;
    fin = fopen( String_val( filename ), "r" );
    if ( fin == NULL ) {
        char exception[MAX_EXCEPTION_MESSAGE_LENGTH];
        snprintf( exception, MAX_EXCEPTION_MESSAGE_LENGTH, "Unable to read from %s", String_val( filename ) );
        caml_invalid_argument( exception );
    }

    CAMLreturn( (value)fin );
}

// Close a file handle, GRIB-style
value ml_grib_close_file( value file ) {
    CAMLparam1( file );

    fclose( File_val( file ) );

    CAMLreturn( Val_unit );
}

// Get a new GRIB handle from a file
value ml_grib_handle_new_from_file( value file ) {
    CAMLparam1( file );

    int error;
    grib_handle *handle;

    handle = grib_handle_new_from_file( NULL, File_val( file ), &error );

    if ( handle == NULL && error != 0 ) {
        char exception[MAX_EXCEPTION_MESSAGE_LENGTH];
        snprintf( exception, MAX_EXCEPTION_MESSAGE_LENGTH, "Error %d retrieving a GRIB message from the file", error );
        caml_invalid_argument( exception );
    }

    if ( handle == NULL ) {
        CAMLreturn( Val_none );
    }
    else {
        CAMLreturn( Val_some( (value)handle ) );
    }
}

//
//
// Working with GRIB index values
//
//

// Create a new index from a set a keys
value ml_grib_index_new( value keys ) {
    CAMLparam1( keys );

    grib_index *index;
    int error;

    index = grib_index_new( NULL, String_val( keys ), &error );

    GRIB_CHECK( error, 0 );

    CAMLreturn( (value)index );
}

// Create a new index from a file
value ml_grib_index_new_from_file( value filename, value keys ) {
    CAMLparam2( filename, keys );

    grib_index *index;
    int error;

    index = grib_index_new_from_file( NULL, String_val( filename ),
                                      String_val( keys ), &error );
    GRIB_CHECK( error, 0 );

    CAMLreturn( (value)index );
}

// Add a file to an index
value ml_grib_index_add_file( value index, value filename ) {
    CAMLparam2( index, filename );

    GRIB_CHECK( grib_index_add_file( Index_val( index ), String_val( filename ) ), 0 );

    CAMLreturn( Val_unit );
}

// Delete an index
value ml_grib_index_delete( value index ) {
    CAMLparam1( index );

    grib_index_delete( Index_val( index ) );

    CAMLreturn( Val_unit );
}

// Select double-valued key(s)
value ml_grib_index_select_double( value index, value key, value val ) {
    CAMLparam3( index, key, val );

    GRIB_CHECK( grib_index_select_double( Index_val( index ), String_val( key ),
                                          Double_val( val ) ), 0 );

    CAMLreturn( Val_unit );
}

// Select long-valued key(s)
value ml_grib_index_select_long( value index, value key, value val ) {
    CAMLparam3( index, key, val );

    GRIB_CHECK( grib_index_select_long( Index_val( index ), String_val( key ),
                                        Int_val( val ) ), 0 );

    CAMLreturn( Val_unit );
}

// Select string-valued key(s)
value ml_grib_index_select_string( value index, value key, value val ) {
    CAMLparam3( index, key, val );

    GRIB_CHECK( grib_index_select_string( Index_val( index ), String_val( key ),
                                          String_val( val ) ), 0 );

    CAMLreturn( Val_unit );
}

// Get the number of distinct values of the given key in the given index
value ml_grib_index_get_size( value index, value key ) {
    CAMLparam2( index, key );

    size_t size;

    GRIB_CHECK(
            grib_index_get_size( Index_val( index ), String_val( key ), &size ),
            0
    );

    CAMLreturn( Val_int( size ) );
}

// Select the next handle in the current index
value ml_grib_handle_new_from_index( value index ) {
    CAMLparam1( index );

    grib_handle *handle;
    int error;

    handle = grib_handle_new_from_index( Index_val( index ), &error );

    // TODO: Proper OCaml error checking!
    // GRIB_CHECK( error, 0 );

    if ( handle == NULL ) {
        CAMLreturn( Val_none );
    }
    else {
        CAMLreturn( Val_some( (value)handle ) );
    }
}

// Write an index out to a file
value ml_grib_index_write( value index, value filename ) {
    CAMLparam2( index, filename );

    GRIB_CHECK(
            grib_index_write( Index_val( index ), String_val( filename ) ),
            0
    );

    CAMLreturn( Val_unit );
}

// Read an index from a file
value ml_grib_index_read( value filename ) {
    CAMLparam1( filename );

    grib_index *index;
    int error;

    index = grib_index_read( NULL, String_val( filename ), &error );

    GRIB_CHECK( error, 0 );

    CAMLreturn( (value)index );
}

//
//
// Working with GRIB iterators
//
//

// Create a new iterator from a handle
value ml_grib_iterator_new( value handle, value flags ) {
    CAMLparam2( handle, flags );

    int error;
    grib_iterator *iterator;

    iterator = grib_iterator_new( Handle_val( handle ), Int_val( flags ), &error );

    GRIB_CHECK( error, 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( (value)iterator );
}

// Get the next value from an iterator
value ml_grib_iterator_next( value iterator ) {
    CAMLparam1( iterator );
    CAMLlocal1( tuple );

    double lat, lon, v;
    int result;

    result = grib_iterator_next( Iterator_val( iterator ), &lat, &lon, &v );

    // See if there are any values left
    if ( result == 0 ) {
        CAMLreturn( Val_none );
    }
    else {
        // Build the tuple to return to OCaml
        tuple = caml_alloc(3, 0);

        Store_field( tuple, 0, caml_copy_double( lat ) );
        Store_field( tuple, 1, caml_copy_double( lon ) );
        Store_field( tuple, 2, caml_copy_double( v ) );

        CAMLreturn( Val_some( tuple ) );
    }
}

// Get the previous value from an iterator
value ml_grib_iterator_previous( value iterator ) {
    CAMLparam1( iterator );
    CAMLlocal1( tuple );

    double lat, lon, v;
    int result;

    result = grib_iterator_previous( Iterator_val( iterator ), &lat, &lon, &v );

    // See if there are any values left
    if ( result == 0 ) {
        CAMLreturn( Val_none );
    }
    else {
        // Build the tuple to return to OCaml
        tuple = caml_alloc(3, 0);

        Store_field( tuple, 0, caml_copy_double( lat ) );
        Store_field( tuple, 1, caml_copy_double( lon ) );
        Store_field( tuple, 2, caml_copy_double( v ) );

        CAMLreturn( Val_some( tuple ) );
    }
}

// Reset an iterator
value ml_grib_iterator_reset( value iterator ) {
    CAMLparam1( iterator );

    GRIB_CHECK( grib_iterator_reset( Iterator_val( iterator ) ), 0 );

    // TODO: Add OCaml error checking!

    CAMLreturn( Val_unit );
}

// Delete an iterator
value ml_grib_iterator_delete( value iterator ) {
    CAMLparam1( iterator );

    GRIB_CHECK( grib_iterator_delete( Iterator_val( iterator ) ), 0 );

    CAMLreturn( Val_unit );
}

//
//
// Multi-field support
//
//

// Enable support for multiple fields in a single GRIB message
value ml_grib_multi_support_on( value unit ) {
    CAMLparam1( unit );

    grib_multi_support_on( NULL );

    CAMLreturn( Val_unit );
}

// Disable support for multiple fields in a single GRIB message
value ml_grib_multi_support_off( value unit ) {
    CAMLparam1( unit );

    grib_multi_support_off( NULL );

    CAMLreturn( Val_unit );
}

// Create a list of handles from a multi-field message, copying the message
// to avoid GC issues
value ml_grib_handles_new_from_multi_message( value data ) {
    CAMLparam1( data );
    // List accumulator, resulting list
    CAMLlocal3( cons, result, message_copy );

    // GRIB API errors
    int error = 0;

    // Get the message length
    size_t length;
    length = caml_string_length( data );

    // Treat the given string like a raw blob of bytes
    // Make a copy to keep the OCaml garbage collector happy
    void *data_copy;
    data_copy = malloc( length );
    if ( data_copy == NULL )
        caml_failwith( "Failed to allocate memory for GRIB message copy" );
    memcpy( data_copy, (void *) String_val( data ), length );

    // Keep a pointer to the original data copy because the GRIB API modifies
    // the pointer passed to it.
    void *original_pointer;
    original_pointer = data_copy;

    // Start accumulating on an empty list
    result = Val_emptylist;

    // A handle pointing to the original message copy
    grib_handle *handle;

    // Build and return a list of messages which reference the
    // fields in the given multi-field message.

    while ( ( ( handle = grib_handle_new_from_multi_message( NULL, &original_pointer, &length, &error ) ) != NULL) && ( error == 0 ) ) {

        // Get the message copy
        message_copy = ml_grib_get_message_copy( (value)handle );

        // Add the copied message to the OCaml list
        cons = caml_alloc( 2, 0 );

        // Head
        Store_field( cons, 0, message_copy );
        // Tail
        Store_field( cons, 1, result );

        // Point the list to the new head
        result = cons;

        // Delete the handle since we don't need it any longer
        grib_handle_delete( handle );
    }

    // Free the original data copy
    free( data_copy );

    // Check for any residual errors
    GRIB_CHECK( error, 0 );

    CAMLreturn( result );
}

//
//
// Key iterator support
//
//

// Translate an OCaml variant value to a GRIB API key iterator flag
int translate_key_iterator_flag( int caml_flag ) {
    int flag;
    switch( caml_flag ) {
        case 0 : flag = GRIB_KEYS_ITERATOR_ALL_KEYS; break;
        case 1 : flag = GRIB_KEYS_ITERATOR_SKIP_READ_ONLY; break;
        case 2 : flag = GRIB_KEYS_ITERATOR_SKIP_OPTIONAL; break;
        case 3 : flag = GRIB_KEYS_ITERATOR_SKIP_EDITION_SPECIFIC; break;
        case 4 : flag = GRIB_KEYS_ITERATOR_SKIP_CODED; break;
        case 5 : flag = GRIB_KEYS_ITERATOR_SKIP_COMPUTED; break;
        case 6 : flag = GRIB_KEYS_ITERATOR_SKIP_DUPLICATES; break;
        case 7 : flag = GRIB_KEYS_ITERATOR_SKIP_FUNCTION; break;
        default : caml_invalid_argument( "Unhandled keys iterator flag" );
    }

    return flag;
}

// Create a key iterator
value ml_grib_keys_iterator_new( value handle, value flags, value name_space ) {
    CAMLparam3( handle, flags, name_space );

    grib_keys_iterator *iterator;

    // Pass an appropriate namespace value
    char *namespace;
    if ( name_space == Val_none )
        namespace = NULL;
    else
        namespace = String_val( Some_val( name_space ) );

    // The combined filter flags
    int filter_flags;

    // OR the elements of the flags list together
    filter_flags = 0;
    while ( flags != Val_emptylist ) {
        filter_flags =
            filter_flags |
            translate_key_iterator_flag( Int_val( Field( flags, 0 ) ) );
        // Point to the tail of the list for the next loop
        flags = Field( flags, 1 );
    }

    iterator = grib_keys_iterator_new( Handle_val( handle ), filter_flags, namespace );

    CAMLreturn( (value)iterator );
}

// Return the next key if there is another one, otherwise return None
value ml_grib_keys_iterator_next( value iterator ) {
    CAMLparam1( iterator );
    CAMLlocal1( result );

    if ( grib_keys_iterator_next( Keys_val( iterator ) ) ) {
        CAMLreturn( Val_some( caml_copy_string( grib_keys_iterator_get_name( Keys_val( iterator ) ) ) ) );
    }
    else {
        CAMLreturn( Val_none );
    }
}

// Delete/free a key iterator
value ml_grib_keys_iterator_delete( value iterator ) {
    CAMLparam1( iterator );

    grib_keys_iterator_delete( Keys_val( iterator ) );

    CAMLreturn( Val_unit );
}

//
//
// Nearest value(s) support
//
//

// OCaml handler to free a nearest when it is GC'd
void finalize_nearest( value nearest ) {
    grib_nearest_delete( Nearest_val( nearest ) );
    return;
}

// Definition for custom OCaml handler functions
static struct custom_operations nearest_custom_ops = {
    identifier: "grib api nearest handling",
    finalize: finalize_nearest,
    compare: custom_compare_default,
    hash: custom_hash_default,
    serialize: custom_serialize_default,
    deserialize: custom_deserialize_default
};

value Val_nearest(grib_nearest *n) {
    grib_nearest **store;
    value ret;
    ret = caml_alloc_custom(&nearest_custom_ops, sizeof(n), 0, 1);
    store = Data_custom_val(ret);
    *store = n;
    return ret;
}

// Create a new nearest value
value ml_grib_nearest_new( value handle ) {
    CAMLparam1( handle );

    grib_nearest *nearest;
    int error;

    nearest = grib_nearest_new( Handle_val( handle ), &error );

    GRIB_CHECK( error, 0 );

    if ( nearest == NULL ) {
        caml_invalid_argument( "Unable to create nearest" );
    }

    CAMLreturn( Val_nearest( nearest ) );
}

// Find the nearest four points to a given location
value ml_grib_nearest_find( value nearest, value handle, value inlat, value inlon, value flags ) {
    CAMLparam5( nearest, handle, inlat, inlon, flags );
    CAMLlocal5( outlats, outlons, values, distances, indexes );
    CAMLlocal1( tuple );

    int error;
    int c_indexes[4];
    size_t len;
    int i;
    int c_flags, ml_flags;

    len = 4;

    // Translate the OCaml flags in to something the GRIB API will understand
    ml_flags = Int_val( flags );
    c_flags = 0;
    if ( ml_flags != 0 ) {
        if ( ml_flags & 1 ) {
            c_flags = c_flags | GRIB_NEAREST_SAME_POINT;
        }
        if ( ml_flags & 2 ) {
            c_flags = c_flags | GRIB_NEAREST_SAME_GRID;
        }
        if ( ml_flags & 4 ) {
            c_flags = c_flags | GRIB_NEAREST_SAME_DATA;
        }
    }

    // Allocate OCaml float (double) arrays
    outlats = caml_alloc( len * Double_wosize, Double_array_tag );
    outlons = caml_alloc( len * Double_wosize, Double_array_tag );
    values = caml_alloc( len * Double_wosize, Double_array_tag );
    distances = caml_alloc( len * Double_wosize, Double_array_tag );

    // Find the nearest points
    error = grib_nearest_find( Nearest_val( nearest ), Handle_val( handle ),
                               Double_val( inlat ), Double_val( inlon ), c_flags,
                               (double *)outlats, (double *)outlons,
                               (double *)values, (double *)distances,
                               c_indexes, &len );

    GRIB_CHECK( error, 0 );

    // Make sure len comes out the same way it went in...
    GRIB_CHECK( len, 4 );

    // Copy indexes, since OCaml integer arrays are not the same as C integer
    // arrays
    indexes = caml_alloc( len, 0 );
    for ( i = 0; i < len; i++ ) {
        Store_field( indexes, i, Val_int( c_indexes[i] ) );
    }

    // Build a tuple to return to OCaml-land
    tuple = caml_alloc( 5, 0 );

    Store_field( tuple, 0, outlats );
    Store_field( tuple, 1, outlons );
    Store_field( tuple, 2, values );
    Store_field( tuple, 3, distances );
    Store_field( tuple, 4, indexes );

    CAMLreturn( tuple );
}

// Find the data point nearest to each of the given input locations
value ml_grib_nearest_find_multiple( value handle, value is_lsm, value inlats, value inlons ) {
    CAMLparam4( handle, is_lsm, inlats, inlons );
    CAMLlocal5( outlats, outlons, values, distances, indexes );
    CAMLlocal1( tuple );

    int error;
    long npoints;
    int i;

    // How many points?
    npoints = Wosize_val( inlats ) / Double_wosize;

    if ( npoints != Wosize_val( inlons ) / Double_wosize ) {
        // Different numbers of longitudes and latitudes provided
        caml_invalid_argument( "Must have the same number of longitudes and latitudes" );
    }

    int c_indexes[npoints];

    // Allocate OCaml float (double) arrays
    outlats = caml_alloc( npoints * Double_wosize, Double_array_tag );
    outlons = caml_alloc( npoints * Double_wosize, Double_array_tag );
    values = caml_alloc( npoints * Double_wosize, Double_array_tag );
    distances = caml_alloc( npoints * Double_wosize, Double_array_tag );

    error = grib_nearest_find_multiple( Handle_val( handle ), Int_val( is_lsm ),
                                        (double *)inlats, (double *)inlons, npoints,
                                        (double *)outlats, (double *)outlons,
                                        (double *)values, (double *)distances,
                                        c_indexes );
    GRIB_CHECK( error, 0 );

    // Copy indexes, since OCaml integer arrays are not the same as C integer
    // arrays
    indexes = caml_alloc( npoints, 0 );
    for ( i = 0; i < npoints; i++ ) {
        Store_field( indexes, i, Val_int( c_indexes[i] ) );
    }

    // Build a tuple to return to OCaml-land
    tuple = caml_alloc( 5, 0 );

    Store_field( tuple, 0, outlats );
    Store_field( tuple, 1, outlons );
    Store_field( tuple, 2, values );
    Store_field( tuple, 3, distances );
    Store_field( tuple, 4, indexes );

    CAMLreturn( tuple );
}

