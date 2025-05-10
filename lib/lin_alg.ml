type t = { matrix : int list list; rows : int; columns : int }

exception InvalidDimensions
exception OutOfBound

let create lst =
  {
    matrix = lst;
    rows = List.length lst;
    columns = List.length (List.nth lst 0);
  }

let transpose t =
  let rec transpose_aux m =
    match m with
    | [] | [] :: _ -> []
    | _ -> List.map List.hd m :: transpose_aux (List.map List.tl m)
  in
  let transposed_matrix = transpose_aux t.matrix in
  { matrix = transposed_matrix; rows = t.columns; columns = t.rows }

let rec add_helper mat1 mat2 =
  let rec sum_list l1 l2 =
    match (l1, l2) with
    | [], [] -> []
    | _ :: _, [] -> raise InvalidDimensions
    | [], _ :: _ -> raise InvalidDimensions
    | h1 :: t1, h2 :: t2 -> (h1 + h2) :: sum_list t1 t2
  in
  match (mat1, mat2) with
  | [], [] -> []
  | _ :: _, [] -> raise InvalidDimensions
  | [], _ :: _ -> raise InvalidDimensions
  | h1 :: t1, h2 :: t2 -> sum_list h1 h2 :: add_helper t1 t2

let add t1 t2 = { t1 with matrix = add_helper t1.matrix t2.matrix }

let dot_helper arr1 arr2 =
  let r, d, c = (Array.length arr1, Array.length arr2, Array.length arr2.(0)) in
  let arr = Array.init_matrix r c (fun r c -> 0) in
  for i = 0 to r - 1 do
    for j = 0 to d - 1 do
      let x = arr1.(i).(j) in
      for k = 0 to c - 1 do
        let y = arr2.(j).(k) in
        arr.(i).(k) <- arr.(i).(k) + (x * y)
      done
    done
  done;
  arr

let rec matrix_of_list_list lst =
  match lst with
  | [] -> [||]
  | h :: t -> Array.append [| Array.of_list h |] (matrix_of_list_list t)

let rec list_list_of_matrix mat =
  Array.to_list (Array.map (fun e -> Array.to_list e) mat)

let dot t1 t2 =
  if t1.columns <> t2.rows then raise InvalidDimensions
  else
    let arr =
      dot_helper (matrix_of_list_list t1.matrix) (matrix_of_list_list t2.matrix)
    in
    { matrix = list_list_of_matrix arr; rows = t1.rows; columns = t2.columns }

let sum_row row = List.fold_left ( + ) 0 row

let sum ?(axis = None) t =
  let new_matrix, new_rows, new_cols =
    match axis with
    | None -> ([ [ sum_row (List.map sum_row t.matrix) ] ], 1, 1)
    | Some 0 ->
        (* sum along rows: returns a list of row sums *)
        (List.map (fun x -> [ x ]) (List.map sum_row t.matrix), t.rows, 1)
    | Some 1 ->
        (* sum along columns: transpose first, then sum rows *)
        ([ List.map sum_row (transpose t).matrix ], 1, t.columns)
    | _ -> raise OutOfBound
  in
  { matrix = new_matrix; rows = new_rows; columns = new_cols }

let scalar_mul_helper mat c =
  List.map (fun e -> List.map (fun f -> f * c) e) mat

let scalar_mul t c = { t with matrix = scalar_mul_helper t.matrix c }
let to_list t = t.matrix
let shape t = (t.rows, t.columns)

let get t r c =
  try List.nth (List.nth t.matrix r) c with Failure x -> raise OutOfBound
