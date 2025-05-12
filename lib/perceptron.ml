open Lin_alg
open Data

type t = {
  data : Data.t;
  steps : int ref;
  max_step : int;
  weight : Lin_alg.t ref;
  bias : int ref;
}

let init data_set max_step =
  {
    data = data_set;
    steps = ref 0;
    max_step;
    weight =
      ref (create [ Array.to_list (Array.make (get_dimension data_set) 0) ]);
    bias = ref 0;
  }

let get_weight perceptron = !(perceptron.weight)
let get_bias perceptron = !(perceptron.bias)

let predict perceptron x =
  let output =
    get (dot (transpose (get_weight perceptron)) x) 0 0 + get_bias perceptron
  in
  if output >= 0 then positive else negative

let step perceptron x y =
  incr perceptron.steps;



  (* pull out the two matrices and compute their shapes *)
  let w = get_weight perceptron in
  let u = scalar_mul x (int_of_label y) in
  let (wr, wc) = shape w        (* or (rows w, cols w) *)
  and (ur, uc) = shape u in     (* or (rows u, cols u) *)
  (* print them *)
  Printf.printf
    ">> about to add: weight is %dx%d, update‐matrix is %dx%d\n%!"
    wr wc ur uc;


  if Hashtbl.find (get_data_set perceptron.data) x <> y then (
    perceptron.weight :=
      add (get_weight perceptron) (scalar_mul x (int_of_label y));
    perceptron.bias := get_bias perceptron + int_of_label y;
    false)
  else true

let rec train_helper perceptron data_set =
  match data_set with
  | [] -> true
  | (tensor, label) :: t ->
      if !(perceptron.steps) < perceptron.max_step then
        step perceptron tensor label && train_helper perceptron t
      else true

let rec train_loop perceptron =
  let no_error = train_helper perceptron (data_to_list perceptron.data) in
  if (not no_error) && !(perceptron.steps) < perceptron.max_step then
    train_loop perceptron

let train perceptron = train_loop perceptron
