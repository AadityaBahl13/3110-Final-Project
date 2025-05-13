open OUnit2
open Test_data
open Test_linalg
open Test_perceptron

let _ = run_test_tt_main ("suite" >::: Test_data.tests @ Test_linalg.tests @ Test_perceptron.tests)
