open OUnit2
open Test_data
open Test_linalg

let _ = run_test_tt_main ("suite" >::: Test_data.tests @ Test_linalg.tests)
