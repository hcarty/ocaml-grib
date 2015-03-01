open OUnit2

let () =
  run_test_tt_main (
    test_list [
      G2clib_test.t;
      Gc_test.t;
      Get_set.t;
      Handle_test.t;
      Index_test.t;
      Message_test.t;
      Nearest_test.t;
    ]
  )
