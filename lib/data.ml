open Bogue
open Lin_alg

type key = int list
type tensor = Lin_alg.t

type label =
  | Positive
  | Negative

let positive = Positive
let negative = Negative

let int_of_label label =
  match label with
  | Positive -> 1
  | Negative -> -1

type t = {
  data_set : (tensor, label) Hashtbl.t;
  dimension : int ref;
}
(** AF: [type t] represents a collection of feature vectors and their
    corresponding labels, where the feature vectors serve as the keys of the
    hashtable of [type t] and the labels serve as the values associated with the
    keys.

    RI: All keys must be feature vectors of the same dimension. *)

(** [check_csv_format loaded_file] is true if all lists in [loaded_file] are of
    the same length and the first value in each of the lists is either 1 or -1.
    It is false otherwise. *)

let get_data_set (data : t) = data.data_set
let get_dimension (data : t) = !(data.dimension)

let rec check_csv_format loaded_file =
  match loaded_file with
  | [] -> true
  | h1 :: t1 ->
      (List.nth h1 0 = 1 || List.nth h1 0 = -1)
      && (match t1 with
         | [] -> true
         | h2 :: t2 -> List.length h1 = List.length h2)
      && check_csv_format t1

(** [int_list_of_string_list lst] is the list [lst] with all of the elements
    converted from type [string] to [int]

    Raises: [Failure "There are non-numerical values in this file"] if there are
    non-numerical elements in lst. *)
let rec int_list_of_string_list lst =
  try
    match lst with
    | [] -> []
    | h :: t -> int_of_string h :: int_list_of_string_list t
  with Failure x -> failwith "There are non-numerical values in this file"

(** [populate_data data lst] adds each list in [lst] to [data] as a key value
    pair, where the value is the head of the list, and the key is the tail. *)
let rec populate_data (data : t) lst =
  match lst with
  | [] -> ()
  | h1 :: t1 -> (
      match h1 with
      | [] -> ()
      | h2 :: t2 ->
          let key = create [ t2 ] in
          let label = if h2 > 0 then Positive else Negative in
          Hashtbl.add (get_data_set data) key label;
          populate_data data t1)

(** [read_from_csv file] is a collection of all label/feature vector pairs
    loaded from the file at location [file]. *)
let read_from_csv file : t =
  try
    let loaded_file = List.map int_list_of_string_list (Csv.load file) in
    let data =
      { data_set = Hashtbl.create (List.length loaded_file); dimension = ref 0 }
    in
    let _ =
      if check_csv_format loaded_file then (
        populate_data data loaded_file;
        if loaded_file <> [] then
          data.dimension := List.length (List.hd loaded_file) - 1)
      else
        failwith
          (file
         ^ ": Invalid file structure. Data points are of different dimensions \
            or labels are not of the form +1/-1")
    in
    data
  with
  | Failure str -> failwith str
  | Csv.Failure (x, y, str) -> failwith str
  | Sys_error str -> failwith str
  | _ -> failwith "An unknown error occured"

(* Property-test helpers *)
let label_to_string (_, label) =
  match label with
  | Positive -> "P"
  | Negative -> "N"

(* Count the number of positive and negative labels *)
let count_labels data =
  Hashtbl.fold
    (fun _ label (pos, neg) ->
      match label with
      | Positive -> (pos + 1, neg)
      | Negative -> (pos, neg + 1))
    (get_data_set data) (0, 0)

let get_key (key : key) : int list = key

let color_of_label = function
  | Positive -> Draw.blue
  | Negative -> Draw.red

let data_to_list (data : t) =
  Hashtbl.fold
    (fun key label accumulator -> (key, label) :: accumulator)
    (get_data_set data) []

let list_to_data_set lst table =
  match lst with
  | [] -> ()
  | (tensor, label) :: t -> Hashtbl.add table tensor label

let list_to_data lst =
  let table = Hashtbl.create (List.length lst) in
  list_to_data_set lst table;
  let dimension = snd (shape (fst (List.hd lst))) in
  { data_set = table; dimension = ref dimension }

let init_data = { data_set = Hashtbl.create 10; dimension = ref 0 }

let filter (data : t) (p : Lin_alg.t -> bool) : t =
  (* Create a fresh table with the same initial size as the original *)
  let old_ht = data.data_set in
  let new_ht = Hashtbl.create (Hashtbl.length old_ht) in

  (* Copy over only those entries whose key satisfies p *)
  Hashtbl.iter
    (fun feature_vec label ->
      if p feature_vec then Hashtbl.add new_ht feature_vec label)
    old_ht;

  (* Preserve the dimension (all feature‐vectors have the same length) *)
  { data_set = new_ht; dimension = ref !(data.dimension) }
