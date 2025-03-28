open OUnit2
open Finalproject.Data
open QCheck
open OUnit2

(* Helper function to create a mock CSV file for testing *)
let create_mock_csv_file data filename =
  let csv_data = List.map (fun row -> List.map string_of_int row) data in
  Csv.save filename csv_data

let test_count_labels file expected =
  let table = read_from_csv file in
  fun _ -> assert_equal expected (count_labels table)

(* Test reading an invalid CSV with incorrect labels *)
let test_read_from_csv_invalid _ =
  let file = "test_invalid.csv" in
  let data = [ [ 1; 2; 3; 1 ]; [ -1; 5; 6; -1 ]; [ 100; 8; 9; 100 ] ] in
  create_mock_csv_file data file;
  try
    let _ = read_from_csv file in
    assert_failure "Expected failure due to invalid label"
  with
  | Failure _ -> ()
  | _ -> assert_failure "Unexpected exception"

let tests =
  "suite"
  >::: [
         "test_count_labels" >:: test_count_labels "../data/valid.csv" (1, 1);
         "test_read_from_csv_invalid" >:: test_read_from_csv_invalid;
         "test_read_from_csv_empty"
         >:: test_count_labels "../data/test_empty.csv" (0, 0);
       ]

(* let tests = "test suite" >::: [ ("basic" >:: fun _ -> assert_equal 0 0) ] *)
let _ = run_test_tt_main tests
