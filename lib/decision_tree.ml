open Data
open Lin_alg

type split = Lin_alg.t -> bool
(** AF: [type split] is a type of function that outputs a boolean by comparing
    the value at some predetermined dimension of a tensor to a predetermined
    split value.

    RI: [type split] functions must use the value in exactly one consistent
    dimension of the input tensor, and must compare this value against one
    consistent split value. Must return true if the tensor's value is greater
    than the split value and false otherwise *)

type t =
  | Leaf of Data.label
  | Tree of tree
      (** AF: [type t] reprfesents a decision tree that subdivides data via
          splits upon specific dimensions in order to create a piecewise linear
          decision boundary

          RI: When a tree subdivides data via a function [f] of [type split],
          all data that satisfies [f data = true] is further subdivided in the
          right subtree. The remaining data is subdivided in the left subtree.
      *)

and tree = {
  data : Data.t;
  split : split ref;
  split_value : int option ref;
  split_dim : int option ref;
  data_size : int;
  entropy : float;
  max_splits : int;
  left : t ref;
  right : t ref;
}
(** AF: [type tree] represents a non-leaf node of a decision tree. This node has
    data_set [data] of size [data_size], and splits this data using function
    [split] which compares tensor valus at dimension [split_dim] against value
    [split_value]. [entropy] is the entropy of the node without splitting the
    data. [max_splits] is the maximum number of splits that are allowed from
    this node in the tree. Finally, [left] and [right] are references to the
    left and right subtrees of this node.

    RI: [entropy] must be greater than 0, or else there would be no point in
    having a non-leaf node as opposed to a leaf node for this set of data. The
    size of [data] must be greater than 1. *)

let default_split : split = fun e -> true
let tup_sum t = fst t + snd t
let slack = 1e-6
let get_left tree = !(tree.left)
let get_right tree = !(tree.right)

let get_split_dim_and_val tree =
  match (!(tree.split_dim), !(tree.split_value)) with
  | Some d, Some v -> (d, v)
  | _ -> (0, min_int)

let rec init_decision_tree data_set max_splits =
  (* count_labels : Data.t -> int * int *)
  let pos_count, neg_count = Data.count_labels data_set in
  let data_size = tup_sum (pos_count, neg_count) in
  (* BASE CASE: no splits left or trivial dataset *)
  if max_splits <= 0 || data_size <= 1 || pos_count = 0 || neg_count = 0 then
    let label = if pos_count >= neg_count then positive else negative in
    Leaf label
  else
    let e = entropy data_set data_size default_split in
    Tree
      {
        data = data_set;
        split = ref default_split;
        split_value = ref None;
        split_dim = ref None;
        data_size;
        entropy = e;
        max_splits;
        left = ref (Leaf positive);
        right = ref (Leaf negative);
      }

and entropy (all_data : Data.t) data_size (splt : split) : float =
  (* SHORT‐CIRCUIT trivial cases *)
  if data_size <= 1 then 0.0
  else
    (* 1) Partition the data *)
    let left_data = Data.filter all_data splt in
    let right_data = Data.filter all_data (fun x -> not (splt x)) in

    (* 2) Directly count labels—no more recursive init! *)
    let pos_l, neg_l = Data.count_labels left_data in
    let pos_r, neg_r = Data.count_labels right_data in

    (* 3) Convert to floats *)
    let n_l = float_of_int (pos_l + neg_l)
    and n_r = float_of_int (pos_r + neg_r)
    and n_tot = float_of_int data_size in

    (* 4) Binary entropy helper with a tiny epsilon guard *)
    let binary_entropy pos neg =
      let s = float_of_int (pos + neg) in
      if s <= slack then 0.0
      else
        let p_pos = float_of_int pos /. s and p_neg = float_of_int neg /. s in
        let term p = if p <= slack then 0.0 else -.p *. log p in
        term p_pos +. term p_neg
    in

    let hL = binary_entropy pos_l neg_l in
    let hR = binary_entropy pos_r neg_r in

    (* 5) Weighted sum *)
    (n_l /. n_tot *. hL) +. (n_r /. n_tot *. hR)

let rec predict t label =
  match t with
  | Leaf y -> y
  | Tree tree -> begin
      let left_or_right = !(tree.split) label in
      if left_or_right then predict !(tree.right) label
      else predict !(tree.left) label
    end

let rec split_data_helper l_list r_list (data : (tensor * label) list)
    (split : split) =
  match data with
  | [] -> ()
  | (tensor, label) :: t -> begin
      let left_or_right = split tensor in
      if left_or_right then r_list := (tensor, label) :: !r_list
      else l_list := (tensor, label) :: !l_list;
      split_data_helper l_list r_list t split
    end

let split_data data split =
  let l_data_list = ref [] in
  let r_data_list = ref [] in
  split_data_helper l_data_list r_data_list data split;
  let l_data = list_to_data !l_data_list in
  let r_data = list_to_data !r_data_list in
  (l_data, r_data)

let rec find_split_helper best_entropy best_split best_val best_dim data_list
    data data_size dimension =
  match data_list with
  | [] -> (best_entropy, best_split, best_val, best_dim)
  | tensor1 :: [] -> (best_entropy, best_split, best_val, best_dim)
  | tensor1 :: tensor2 :: t -> begin
      let x1_list, x2_list =
        (List.hd (to_list (fst tensor1)), List.hd (to_list (fst tensor2)))
      in
      let current_val =
        (List.nth x1_list dimension + List.nth x2_list dimension) / 2
      in
      let current_split = fun tensor -> get tensor 0 dimension > current_val in
      let current_entropy = entropy data data_size current_split in
      if
        current_entropy <= best_entropy
        && current_val <> List.nth x2_list dimension
      then
        find_split_helper current_entropy current_split (Some current_val)
          (Some dimension) (tensor2 :: t) data data_size dimension
      else
        find_split_helper best_entropy best_split best_val best_dim
          (tensor2 :: t) data data_size dimension
    end

let find_split t =
  match t with
  | Leaf y -> (default_split, None, None)
  | Tree tree -> begin
      let best_entropy = ref tree.entropy in
      let best_split = ref default_split in
      let best_val = ref None in
      let best_dim = ref None in
      let dimensions = get_dimension tree.data in
      for d = 0 to dimensions - 1 do
        let new_entropy, new_split, new_val, new_dim =
          let unsorted = data_to_list tree.data in
          let sorted_d =
            List.sort
              (fun (x1, _) (x2, _) ->
                begin
                  let x1_lst = List.hd (to_list x1) in
                  let x2_lst = List.hd (to_list x2) in
                  List.nth x1_lst d - List.nth x2_lst d
                end)
              unsorted
          in
          find_split_helper !best_entropy !best_split !best_val !best_dim
            sorted_d tree.data tree.data_size d
        in
        if new_entropy <= !best_entropy then (
          best_entropy := new_entropy;
          best_split := new_split;
          best_val := new_val;
          best_dim := new_dim)
      done;
      (!best_split, !best_val, !best_dim)
    end

let split tree =
  match tree with
  | Leaf y -> ()
  | Tree t -> begin
      let opt_split, opt_val, opt_dim = find_split tree in
      t.split := opt_split;
      t.split_value := opt_val;
      t.split_dim := opt_dim;
      let l_data, r_data = split_data (data_to_list t.data) opt_split in
      if
        Float.abs (entropy t.data t.data_size opt_split -. 0.) < slack
        || t.max_splits = 1 || opt_val = None
      then (
        let l_pos, l_neg = count_labels l_data in
        let r_pos, r_neg = count_labels r_data in
        if l_neg > l_pos then t.left := Leaf negative;
        if r_pos > r_neg then t.right := Leaf positive)
      else
        let l_size = tup_sum (count_labels l_data) in
        let l_entropy = entropy l_data l_size default_split in
        (if Float.abs l_entropy > slack then
           t.left :=
             Tree
               {
                 data = l_data;
                 split = ref default_split;
                 split_value = ref None;
                 split_dim = ref None;
                 data_size = l_size;
                 entropy = entropy l_data l_size default_split;
                 max_splits = t.max_splits - 1;
                 left = ref (Leaf positive);
                 right = ref (Leaf negative);
               }
         else
           let l_pos, l_neg = count_labels l_data in
           if l_neg > l_pos then t.left := Leaf negative);
        let r_size = tup_sum (count_labels r_data) in
        let r_entropy = entropy r_data r_size default_split in
        if Float.abs r_entropy > slack then
          t.right :=
            Tree
              {
                data = r_data;
                split = ref default_split;
                split_value = ref None;
                split_dim = ref None;
                data_size = r_size;
                entropy = entropy r_data r_size default_split;
                max_splits = t.max_splits - 1;
                left = ref (Leaf positive);
                right = ref (Leaf negative);
              }
        else
          let r_pos, r_neg = count_labels r_data in
          if r_pos > r_neg then t.right := Leaf positive
    end

let rec train tree =
  match tree with
  | Leaf y -> ()
  | Tree t ->
      split tree;
      train !(t.left);
      train !(t.right)
