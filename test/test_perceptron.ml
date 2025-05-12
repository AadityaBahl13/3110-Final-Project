open OUnit2
open Finalproject.Lin_alg
open Finalproject.Data
open Finalproject.Perceptron
open QCheck

(* Perceptron unit‐tests *)

open Finalproject.Perceptron

(* helper to load our tiny CSV fixtures *)
let load_single () =
  let data = read_from_csv "../data/test_data_single.csv" in
  let x, lbl = List.hd (data_to_list data) in
  (data, x, lbl)

(* 1) init: zero weight & zero bias, correct shape *)
let perceptron_init_test =
  "Perceptron.init" >:: fun _ ->
    let data = read_from_csv "../data/test_data_single.csv" in
    let p = init data 5 in
    let w = get_weight p in
    let (r,c) = shape w in
    (* one row, one column per feature *)
    assert_equal ~msg:"rows" 1 r;
    assert_equal ~msg:"cols" (get_dimension data) c;
    for i = 0 to r-1 do
      for j = 0 to c-1 do
        assert_equal
          ~msg:(Printf.sprintf "w[%d,%d]" i j)
          0
          (get w i j)
      done
    done;
    assert_equal ~msg:"bias" 0 (get_bias p)

(* 2) step returns true on correct label, no change *)
let perceptron_step_correct_test =
  "Perceptron.step correct" >:: fun _ ->
    let data, x, lbl = load_single () in
    let p = init data 3 in
    let w0 = get_weight p and b0 = get_bias p in
    assert_bool "should be true" (step p x lbl);
    assert_equal ~msg:"w unchanged" w0 (get_weight p);
    assert_equal ~msg:"b unchanged" b0 (get_bias p)

(* 3) step returns false on wrong label, updates w & b *)
let perceptron_step_incorrect_test =
  "Perceptron.step incorrect" >:: fun _ ->
    let data, x, lbl = load_single () in
    let p = init data 3 in
    let wrong = if lbl = positive then negative else positive in
    let b0 = get_bias p in
    assert_bool "should be false" (not (step p x wrong));
    let b1 = get_bias p in
    assert_equal
      ~msg:"bias updated"
      (b0 + int_of_label wrong)
      b1;
    (* some entry of w must now be nonzero *)
    let w1 = get_weight p in
    let (r,c) = shape w1 in
    let updated = ref false in
    for i = 0 to r-1 do
      for j = 0 to c-1 do
        if get w1 i j <> 0 then updated := true
      done
    done;
    assert_bool "w updated" !updated

(* 4) predict obeys current w·x + b decision boundary *)
let perceptron_predict_test =
  "Perceptron.predict" >:: fun _ ->
    let data, x, _ = load_single () in
    let p = init data 2 in
    (* 0·x+0 = 0 ⇒ positive *)
    assert_equal ~msg:"initial" positive (predict p x);
    (* force a negative step *)
    ignore (step p x negative);
    assert_equal ~msg:"after update" negative (predict p x)

(* 5) train converges on two‐point separable dataset *)
let perceptron_train_test =
  "Perceptron.train" >:: fun _ ->
    let data = read_from_csv "../data/test_data_train.csv" in
    let p = init data 10 in
    train p;
    List.iter
      (fun (x,lbl) ->
        assert_equal
          ~msg:(Printf.sprintf "classify %s" (label_to_string (x,lbl)))
          lbl
          (predict p x))
      (data_to_list data)


(*------------------------------------------------------------------*)
let tests = [ 
   perceptron_init_test;
   perceptron_step_correct_test;
   perceptron_step_incorrect_test;
]



