type t = { matrix : int list list; rows : int; columns : int }

let transpose t =
  let rec transpose_aux m =
    match m with
    | [] | [] :: _ -> []
    | _ -> List.map List.hd m :: transpose_aux (List.map List.tl m)
  in
  let transposed_matrix = transpose_aux t.matrix in
  { matrix = transposed_matrix; rows = t.columns; columns = t.rows }

let sum_row row = List.fold_left ( + ) 0 row

let sum t ?axs =
  let new_matrix, new_rows, new_cols =
    match axs with
    | None -> ([[sum_row (List.map sum_row t.matrix)]], 1, 1)
    | Some 0 ->
        (* sum along rows: returns a list of row sums *)
        (List.map (fun x -> [x]) (List.map sum_row t.matrix), t.rows, 1)
    | Some 1 ->
        (* sum along columns: transpose first, then sum rows *)
        ([(List.map sum_row (transpose t).matrix)], 1, t.columns)

    | _ -> failwith "Invalid axis: must be 0 (rows) or 1 (columns)"
  in
  { matrix = new_matrix; rows = new_rows; columns = new_cols }
