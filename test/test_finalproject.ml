open OUnit2
open Test_data
open Test_linalg
open Test_perceptron
open Test_decision_tree

let _ = run_test_tt_main ("suite" >::: Test_data.tests @ Test_linalg.tests @ Test_perceptron.tests @ Test_decision_tree.tests)

