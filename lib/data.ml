open Lin_alg

type key = Lin_alg.t
type label = Positive | Negative

type t = (key, label) Hashtbl.t
(** AF: [type t] represents a collection of feature vectors and their
    corresponding labels, where the feature vectors serve as the keys of the
    hashtable of [type t] and the labels serve as the values associated with the
    keys.

    RI: All keys must be feature vectors of the same dimension. *)

(** [check_csv_format loaded_file] is true if all lists in [loaded_file] are of
    the same length and the first value in each of the lists is either 1 or -1.
    It is false otherwise. *)
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

(** [populate_table table lst] adds each list in [lst] to [table] as a key value
    pair, where the value is the head of the list, and the key is the tail. *)
let rec populate_table (table : t) lst =
  match lst with
  | [] -> ()
  | h1 :: t1 -> (
      match h1 with
      | [] -> ()
      | h2 :: t2 ->
          let key = create [ t2 ] in
          let label = if h2 > 0 then Positive else Negative in
          Hashtbl.add table key label;
          populate_table table t1)

(** [read_from_csv file] is a collection of all label/feature vector pairs
    loaded from the file at location [file]. *)
let read_from_csv file : t =
  try
    let loaded_file = List.map int_list_of_string_list (Csv.load file) in
    let table = Hashtbl.create (List.length loaded_file) in
    let _ =
      if check_csv_format loaded_file then populate_table table loaded_file
      else
        failwith
          (file
         ^ ": Invalid file structure. Data points are of different dimensions \
            or labels are not of the form +1/-1")
    in
    table
  with
  | Failure str -> failwith str
  | Csv.Failure (x, y, str) -> failwith str
  | Sys_error str -> failwith str
  | _ -> failwith "An unknown error occured"

(* Property-test helpers *)
let label_to_string (_, label) =
  match label with Positive -> "P" | Negative -> "N"

(* Count the number of positive and negative labels *)
let count_labels table =
  Hashtbl.fold
    (fun _ label (pos, neg) ->
      match label with Positive -> (pos + 1, neg) | Negative -> (pos, neg + 1))
    table (0, 0)

let table_to_list (table : t) =
  Hashtbl.fold (fun key label acc -> (key, label) :: acc) table []
