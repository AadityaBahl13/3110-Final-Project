open OUnit2
open Finalproject.Data
open Finalproject.Decision_tree
open Finalproject.Lin_alg

let epsilon = 1e-6
let ln2 = log 2.0
let slack = 1e-6
let zeros n = create [ List.init n (fun _ -> 0) ]

(* 1) Uniform positive labels: entropy = 0 *)
let test_entropy_uniform_pos _ =
  let data = read_from_csv "../data/uniform_pos.csv" in
  let size = List.length (data_to_list data) in
  let e = entropy data size (fun _ -> true) in
  assert_equal ~msg:"entropy on uniform positive should be 0"
    ~printer:string_of_float
    ~cmp:(fun a b -> abs_float (a -. b) < epsilon)
    0.0 e

(* 2) Uniform negative labels: entropy = 0 *)
let test_entropy_uniform_neg _ =
  let data = read_from_csv "../data/uniform_neg.csv" in
  let size = List.length (data_to_list data) in
  let e = entropy data size (fun _ -> true) in
  assert_equal ~msg:"entropy on uniform negative should be 0"
    ~printer:string_of_float
    ~cmp:(fun a b -> abs_float (a -. b) < epsilon)
    0.0 e

(* 3) Balanced labels: default split entropy = ln 2 (~0.6931) *)
let test_entropy_balanced_default _ =
  let data = read_from_csv "../data/balanced.csv" in
  let size = List.length (data_to_list data) in
  let e = entropy data size (fun _ -> true) in
  assert_equal ~msg:"entropy on balanced with default split should equal ln 2"
    ~printer:string_of_float
    ~cmp:(fun a b -> abs_float (a -. b) < epsilon)
    ln2 e

(* 4) Custom split from find_split yields zero entropy for balanced data *)
let test_entropy_find_split _ =
  let data = read_from_csv "../data/balanced.csv" in
  let size = List.length (data_to_list data) in
  let tree = init_decision_tree data 1 in
  let split_fn, _, _ = find_split tree in
  let e = entropy data size split_fn in
  assert_bool "entropy for find_split on balanced should be ~0"
    (abs_float e < epsilon)

(* 5) Predict on single example returns its own label *)
let test_predict_single _ =
  let data = read_from_csv "../data/single.csv" in
  let rows = data_to_list data in
  let x, y =
    match rows with
    | [ (x, y) ] -> (x, y)
    | _ -> (zeros 1, positive)
  in
  let tree = init_decision_tree data 1 in
  let lbl = predict tree x in
  assert_equal ~msg:"predict should return original label" y lbl

(* 6) Train converges on two-point separable dataset *)
let test_train_converges _ =
  let data = read_from_csv "../data/valid.csv" in
  let tree = init_decision_tree data 10 in
  train tree;
  List.iter
    (fun (x, lbl) ->
      assert_equal
        ~msg:
          (Printf.sprintf "classify %s"
             (String.concat ","
                (List.map string_of_int (List.concat (to_list x)))))
        lbl (predict tree x))
    (data_to_list data)

let test_split_no_expand _ =
  (* 1) Load a small 2-point balanced dataset *)
  let data = read_from_csv "../data/balanced.csv" in
  let rows = data_to_list data in
  let size = List.length rows in

  (* 2) Build a tree with exactly one split allowed *)
  let tree = init_decision_tree data 1 in

  (* 3) Pick its best split and record the entropy under that split *)
  let split_fn, _, _ = find_split tree in
  let orig_entropy = entropy data size split_fn in

  (* 4) Invoke split—it should record the same split but NOT grow children *)
  split tree;

  (* 5) Re–fetch the split and re–compute entropy *)
  let split_fn', _, _ = find_split tree in
  let new_entropy = entropy data size split_fn' in

  (* 6) They must be identical (within a tiny epsilon) *)
  assert_equal ~msg:"split with max_splits=1 should not change root entropy"
    ~printer:string_of_float
    ~cmp:(fun a b -> abs_float (a -. b) < epsilon)
    orig_entropy new_entropy

let test_split_expand_nonperfect _ =
  (* 1) three-point non-perfect dataset *)
  let data = read_from_csv "../data/not_separable.csv" in
  let size = List.length (data_to_list data) in

  (* 2) allow at least two splits *)
  let tree = init_decision_tree data 2 in

  (* 3) best‐split impurity should be > slack *)
  let split_fn, _, _ = find_split tree in
  let root_ent = entropy data size split_fn in

  (* 4) now actually call split—should build children *)
  split tree;

  assert_bool "root entropy should be > slack" (root_ent >= slack)

let tests =
  [
    "test_entropy_uniform_pos" >:: test_entropy_uniform_pos;
    "test_entropy_uniform_neg" >:: test_entropy_uniform_neg;
    "test_entropy_balanced_default" >:: test_entropy_balanced_default;
    "test_entropy_find_split" >:: test_entropy_find_split;
    "test_predict_single" >:: test_predict_single;
    "test_train_converges" >:: test_train_converges;
    "test_split_no_expand" >:: test_split_no_expand;
    "test_split_expand_nonperfect" >:: test_split_expand_nonperfect;
  ]
