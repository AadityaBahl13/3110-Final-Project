open Lin_alg
open Data

type t = {
  data : Data.t;
  steps : int ref;
  max_step : int;
  weight : Lin_alg.t ref;
  bias : int ref;
}

let init_perceptron data_set max_step =
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
  (* fetch current w, b *)
  let w = get_weight perceptron in
  let b = get_bias perceptron in

  (* now compute score (int) *)
  let dotted = dot w (transpose x) in
  let score = get dotted 0 0 + b in

  if score >= 0 then begin
    positive
  end
  else begin
    negative
  end

let step perceptron x y_true =
  (* bump our step counter *)
  incr perceptron.steps;

  let y_pred = predict perceptron x in

  (* grab current weight and compute the update matrix *)
  let w = get_weight perceptron in
  let u = Lin_alg.scalar_mul x (Data.int_of_label y_true) in

  (* now only update when they differ *)
  if y_true <> y_pred then (
    perceptron.weight := Lin_alg.add w u;
    perceptron.bias := get_bias perceptron + Data.int_of_label y_true;
    false)
  else true

let rec check_perceptron_helper perceptron data_set =
  match data_set with
  | [] -> true
  | (tensor, label) :: t ->
      if predict perceptron tensor = label then
        check_perceptron_helper perceptron t
      else false

let rec check_perceptron perceptron =
  check_perceptron_helper perceptron (data_to_list perceptron.data)

let rec train_helper perceptron data_set =
  match data_set with
  | [] -> true
  | (tensor, label) :: t ->
      if !(perceptron.steps) < perceptron.max_step then
        let step_result = step perceptron tensor label in
        let train_result = train_helper perceptron t in
        step_result && train_result
      else true

let rec train_loop perceptron data_list =
  let no_error = train_helper perceptron data_list in
  if (not no_error) && !(perceptron.steps) < perceptron.max_step then
    train_loop perceptron data_list

let train perceptron = train_loop perceptron (data_to_list perceptron.data)
