open Data

type split = Lin_alg.t -> bool

type t =
  | Leaf of Data.label
  | Tree of tree

and tree = {
  data : Data.t;
  split : split ref;
  data_size : int;
  entropy : float;
  left : t ref;
  right : t ref;
}



open Data

let entropy (tree : t) (splt : split) : float =
  match tree with
  | Leaf _ ->
    (* No further questions → zero entropy *)
    0.0

  | Tree node ->
    let all_data = node.data in

    (* 1) carve out the two subsets without touching node.left/right *)
    let left_data  = Data.filter  all_data splt in
    let right_data = Data.filter  all_data (fun x -> not (splt x)) in

    (* 2) wrap them in throwaway trees so we can count labels *)
    let left_tree  = init_decision_tree left_data  0 in
    let right_tree = init_decision_tree right_data 0 in

    (* 3) pull out (#pos, #neg) from each side *)
    let pos_l, neg_l = count_labels left_tree in
    let pos_r, neg_r = count_labels right_tree in

    (* convert to floats and total *)
    let n_l   = float_of_int (pos_l + neg_l)
    and n_r   = float_of_int (pos_r + neg_r)
    and n_tot = float_of_int node.data_size in

    (* helper: H(p_pos,p_neg) = –p_pos log p_pos – p_neg log p_neg *)
    let binary_entropy pos neg =
      let s = float_of_int (pos + neg) in
      if s = 0. then 0. else
      let p_pos = float_of_int pos /. s in
      let p_neg = float_of_int neg /. s in
      let term p = if p <= 0. then 0. else -. p *. log p in
      term p_pos +. term p_neg
    in

    let hL = binary_entropy pos_l neg_l in
    let hR = binary_entropy pos_r neg_r in

    (* 4) weighted average: (|L|/|T|)·H(L) + (|R|/|T|)·H(R) *)
    if n_tot = 0. then 0.
    else
      (n_l  /. n_tot) *. hL
      +. (n_r  /. n_tot) *. hR


