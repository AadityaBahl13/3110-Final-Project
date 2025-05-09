open Lin_alg

type tensor = Lin_alg.t
type label = Positive | Negative

let positive = Positive
let negative = Negative
let int_of_label label = match label with Positive -> 1 | Negative -> -1

type t = { data_set : (tensor, label) Hashtbl.t; dimension : int ref }
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
          data.dimension := List.length (List.hd loaded_file))
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
  match label with Positive -> "P" | Negative -> "N"

(* Count the number of positive and negative labels *)
let count_labels data =
  Hashtbl.fold
    (fun _ label (pos, neg) ->
      match label with Positive -> (pos + 1, neg) | Negative -> (pos, neg + 1))
    (get_data_set data) (0, 0)

let data_to_list (data : t) =
  Hashtbl.fold (fun key label acc -> (key, label) :: acc) (get_data_set data) []
