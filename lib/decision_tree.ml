open Data
open Lin_alg

type split = Lin_alg.t -> bool

type t =
  | Leaf of Data.label
  | Tree of tree

and tree = {
  data : Data.t;
  split : split ref;
  data_size : int;
  entropy : float;
  max_splits : int;
  left : t ref;
  right : t ref;
}

let default_split : split = fun e -> true
let entropy tree split = 0.
let tup_sum t = fst t + snd t
let slack = 1e-6

let init_decision_tree data_set max_splits =
  Tree
    {
      data = data_set;
      split = ref default_split;
      data_size = tup_sum (count_labels data_set);
      entropy = entropy data_set default_split;
      max_splits;
      left = ref (Leaf positive);
      right = ref (Leaf negative);
    }

let rec predict t label =
  match t with
  | Leaf y -> y
  | Tree tree -> begin
      let left_or_right = !(tree.split) label in
      if left_or_right then predict !(tree.right) label
      else predict !(tree.left) label
    end

let split_data_helper l_list r_list (data : (tensor * label) list)
    (split : split) =
  match data with
  | [] -> ()
  | (tensor, label) :: t -> begin
      let left_or_right = split tensor in
      if left_or_right then r_list := (tensor, label) :: !r_list
      else l_list := (tensor, label) :: !l_list
    end

let split_data data split =
  let l_data_list = ref [] in
  let r_data_list = ref [] in
  let l_data = list_to_data !l_data_list in
  let r_data = list_to_data !r_data_list in
  (l_data, r_data)

let rec find_split_helper best_entropy best_split data_list data dimension =
  match data_list with
  | [] -> (best_entropy, best_split)
  | tensor1 :: [] -> (best_entropy, best_split)
  | tensor1 :: tensor2 :: t -> begin
      let x1_list, x2_list =
        (List.hd (to_list (fst tensor1)), List.hd (to_list (fst tensor2)))
      in
      let split_value =
        (List.nth x1_list dimension + List.nth x2_list dimension) / 2
      in
      let current_split = fun tensor -> get tensor 0 dimension > split_value in
      let current_entropy = entropy data current_split in
      if current_entropy < best_entropy then
        find_split_helper current_entropy current_split (tensor2 :: t) data
          dimension
      else
        find_split_helper best_entropy best_split (tensor2 :: t) data dimension
    end

let find_split t =
  match t with
  | Leaf y -> default_split
  | Tree tree -> begin
      let best_entropy = ref tree.entropy in
      let best_split = ref default_split in
      let dimensions = get_dimension tree.data in
      for d = 0 to dimensions - 1 do
        let new_entropy, new_split =
          find_split_helper !best_entropy !best_split (data_to_list tree.data)
            tree.data d
        in
        if new_entropy < !best_entropy then best_entropy := new_entropy;
        best_split := new_split
      done;
      !best_split
    end

let split tree =
  match tree with
  | Leaf y -> ()
  | Tree t -> begin
      let opt_split = find_split tree in
      t.split := opt_split;
      let l_data, r_data = split_data (data_to_list t.data) opt_split in
      if Float.abs (entropy t.data opt_split -. 0.) < slack || t.max_splits = 1
      then ()
      else (
        t.left :=
          Tree
            {
              data = l_data;
              split = ref default_split;
              data_size = tup_sum (count_labels l_data);
              entropy = entropy l_data default_split;
              max_splits = t.max_splits - 1;
              left = ref (Leaf positive);
              right = ref (Leaf negative);
            };
        t.right :=
          Tree
            {
              data = r_data;
              split = ref default_split;
              data_size = tup_sum (count_labels r_data);
              entropy = entropy r_data default_split;
              max_splits = t.max_splits - 1;
              left = ref (Leaf positive);
              right = ref (Leaf negative);
            })
    end

let rec train tree =
  match tree with
  | Leaf y -> ()
  | Tree t ->
      if Float.abs (t.entropy -. 0.) <= slack || t.max_splits = 0 then ()
      else split tree;
      train !(t.left);
      train !(t.right)
