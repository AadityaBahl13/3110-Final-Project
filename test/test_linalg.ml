open OUnit2
open Finalproject.Lin_alg
open Finalproject.Data
open QCheck

let () = Random.self_init ()

let rec make_matrix lst acc len =
  match lst with
  | [] -> []
  | h :: t ->
      if len > 0 then make_matrix t (acc @ [ h ]) (len - 1)
      else if acc = [] then [ [] ]
      else if List.length t < List.length acc then [ acc ]
      else acc :: make_matrix t [ h ] (List.length acc - 1)

let rec check_get tensor lst t_shape count =
  try
    for i = 0 to fst t_shape - 1 do
      for j = 0 to snd t_shape - 1 do
        if get tensor i j <> List.nth lst !count then failwith "false";
        incr count
      done
    done;
    true
  with Failure x -> false

let get_oracle lst =
  let columns = Random.int_in_range ~min:0 ~max:10 in
  let matrix = make_matrix lst [] columns in
  let tensor = create matrix in
  let t_shape = shape tensor in
  check_get tensor lst t_shape (ref 0)

let rec check_scalar_mul tensor lst t_shape count c =
  try
    for i = 0 to fst t_shape - 1 do
      for j = 0 to snd t_shape - 1 do
        if get tensor i j <> List.nth lst !count * c then failwith "false";
        incr count
      done
    done;
    true
  with Failure x -> false

let scalar_mul_oracle lst =
  let columns = Random.int_in_range ~min:0 ~max:10 in
  let c = Random.int_in_range ~min:(-10) ~max:10 in
  let matrix = make_matrix lst [] columns in
  let tensor = create matrix in
  let t_shape = shape tensor in
  let mul_matrix = scalar_mul tensor c in
  check_scalar_mul mul_matrix lst t_shape (ref 0) c

let transpose_oracle lst =
  let columns = Random.int_in_range ~min:1 ~max:10 in
  let matrix = make_matrix lst [] columns in
  let tensor = create matrix in
  tensor = transpose (transpose tensor)

let rec lst_equality lst1 lst2 =
  match lst1 with
  | [] -> lst2 = []
  | h1 :: t1 -> (
      match lst2 with
      | [] -> false
      | h2 :: t2 -> h1 = h2 && lst_equality t1 t2)

let rec lst_lst_equality lstlst1 lstlst2 =
  match lstlst1 with
  | [] -> lstlst2 = []
  | h1 :: t1 -> (
      match lstlst2 with
      | [] -> false
      | h2 :: t2 -> lst_equality h1 h2 && lst_lst_equality t1 t2)

let to_list_oracle lst =
  let columns = Random.int_in_range ~min:0 ~max:10 in
  let matrix = make_matrix lst [] columns in
  let tensor = create matrix in
  lst_lst_equality matrix (to_list tensor)

let rec sum_lst_lst lst =
  match lst with
  | [] -> 0
  | h :: t -> List.fold_left (fun acc e -> acc + e) 0 h + sum_lst_lst t

let sum_axis_none_oracle lst =
  let columns = Random.int_in_range ~min:0 ~max:10 in
  let matrix = make_matrix lst [] columns in
  let tensor = create matrix in
  let t_sum = sum tensor in
  shape t_sum = (1, 1) && get t_sum 0 0 = sum_lst_lst matrix

let rec sum_rows matrix arr =
  match matrix with
  | [] -> ()
  | h :: t ->
      List.iteri (fun i e -> arr.(i) <- arr.(i) + e) h;
      sum_rows t arr

let check_sum1 t_sum arr bool =
  try List.iteri (fun i e -> bool := !bool && e = arr.(i)) t_sum
  with x -> raise x

let sum_axis1_oracle lst =
  let columns = Random.int_in_range ~min:0 ~max:10 in
  let matrix = make_matrix lst [] columns in
  let tensor = create matrix in
  let t_shape = shape tensor in
  let t_sum = sum ~axis:(Some 1) tensor in
  let bool = ref true in
  let arr = Array.make (List.length (List.hd matrix)) 0 in
  sum_rows matrix arr;
  check_sum1 (List.hd (to_list t_sum)) arr bool;
  shape t_sum = (1, snd t_shape) && !bool

let rec sum_columns matrix =
  match matrix with
  | [] -> []
  | h :: t -> [ List.fold_left (fun acc e -> e + acc) 0 h ] :: sum_columns t

let check_sum0 t_sum matrix =
  List.fold_left2 (fun acc a b -> acc && a = b) true t_sum matrix

let sum_axis0_oracle lst =
  let columns = Random.int_in_range ~min:0 ~max:10 in
  let matrix = make_matrix lst [] columns in
  let tensor = create matrix in
  let t_shape = shape tensor in
  let t_sum = sum ~axis:(Some 0) tensor in
  shape t_sum = (fst t_shape, 1)
  && check_sum0 (to_list t_sum) (sum_columns matrix)

let rec check_add add_t t1 t2 bool =
  match (add_t, t1, t2) with
  | [], [], [] -> ()
  | h_a :: t_a, h1 :: t1, h2 :: t2 ->
      List.iteri
        (fun i e -> bool := !bool && e = List.nth h1 i + List.nth h2 i)
        h_a;
      check_add t_a t1 t2 bool
  | _ -> ()

let add_oracle (mat1, mat2) =
  let t1 = create mat1 in
  let t2 = create mat2 in
  let add_t = add t1 t2 in
  let bool = ref true in
  check_add (to_list add_t) (to_list t1) (to_list t2) bool;
  !bool

let dot_check_1x2_2x1 mat1 mat2 =
  [
    [
      (List.nth (List.nth mat1 0) 0 * List.nth (List.nth mat2 0) 0)
      + (List.nth (List.nth mat1 0) 1 * List.nth (List.nth mat2 1) 0);
    ];
  ]

let dot_oracle_1x2_2x1 (mat1, mat2) =
  let t1 = create mat1 in
  let t2 = create mat2 in
  let dot_t = dot t1 t2 in
  let dot_check = dot_check_1x2_2x1 mat1 mat2 in
  to_list dot_t = dot_check

let dot_check_2x1_1x2 mat1 mat2 =
  [
    [
      List.nth (List.nth mat1 0) 0 * List.nth (List.nth mat2 0) 0;
      List.nth (List.nth mat1 0) 0 * List.nth (List.nth mat2 0) 1;
    ];
    [
      List.nth (List.nth mat1 1) 0 * List.nth (List.nth mat2 0) 0;
      List.nth (List.nth mat1 1) 0 * List.nth (List.nth mat2 0) 1;
    ];
  ]

let dot_oracle_2x1_1x2 (mat1, mat2) =
  let t1 = create mat1 in
  let t2 = create mat2 in
  let dot_t = dot t1 t2 in
  let dot_check = dot_check_2x1_1x2 mat1 mat2 in
  shape dot_t = (fst (shape t1), snd (shape t2)) && to_list dot_t = dot_check

let make_single_matrix_test oracle n name =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~name
       (QCheck.make
          (QCheck.Gen.list_size (QCheck.Gen.int_range n n)
             (QCheck.Gen.int_range 1 100)))
       oracle ~count:100)

let make_two_matrix_test oracle r1 c1 r2 c2 name =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~name
       (QCheck.make
          (QCheck.Gen.tup2
             (QCheck.Gen.list_size
                (QCheck.Gen.int_range r1 r1)
                (QCheck.Gen.list_size
                   (QCheck.Gen.int_range c1 c1)
                   (QCheck.Gen.int_range 1 100)))
             (QCheck.Gen.list_size
                (QCheck.Gen.int_range r2 r2)
                (QCheck.Gen.list_size
                   (QCheck.Gen.int_range c2 c2)
                   (QCheck.Gen.int_range 1 100)))))
       oracle ~count:100)

let get_test name input columns =
  "Get Test: " ^ name >:: fun _ ->
  let tensor = create (make_matrix input [] columns) in
  assert_equal true (check_get tensor input (shape tensor) (ref 0))

let add_0x0_test =
  "Add Test: Empty" >:: fun _ ->
  let t1 = create [ [] ] in
  let t2 = create [ [] ] in
  assert_equal [ [] ] (to_list (add t1 t2))

let tests =
  [
    get_test "naive" [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] 3;
    make_single_matrix_test get_oracle 100 "get";
    make_single_matrix_test transpose_oracle 100 "transpose";
    make_single_matrix_test to_list_oracle 100 "to_list";
    make_single_matrix_test sum_axis_none_oracle 100 "sum_axis_none";
    make_single_matrix_test sum_axis0_oracle 100 "sum_axis0";
    make_single_matrix_test sum_axis1_oracle 100 "sum_axis1";
    make_single_matrix_test scalar_mul_oracle 100 "scalar_mul";
    add_0x0_test;
    (let r, c =
       (Random.int_in_range ~min:1 ~max:10, Random.int_in_range ~min:1 ~max:10)
     in
     make_two_matrix_test add_oracle r c r c "add");
    make_two_matrix_test dot_oracle_1x2_2x1 1 2 2 1 "1x2 2x1 dot";
    make_two_matrix_test dot_oracle_2x1_1x2 2 1 1 2 "2x1 1x2 dot";
  ]
