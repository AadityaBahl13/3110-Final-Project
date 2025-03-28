type key = int list
type label = Positive | Negative
type t = (key, label) Hashtbl.t

let rec check_csv_format loaded_file =
  match loaded_file with
  | [] -> true
  | h1 :: t1 ->
      (List.nth h1 0 = 1 || List.nth h1 0 = -1)
      && (match t1 with
         | [] -> true
         | h2 :: t2 -> List.length h1 = List.length h2)
      && check_csv_format t1

let rec int_list_of_string_list lst =
  try
    match lst with
    | [] -> []
    | h :: t -> int_of_string h :: int_list_of_string_list t
  with Failure x -> failwith "There are non-numerical values in this file"

let rec populate_table (table : t) lst =
  match lst with
  | [] -> ()
  | h1 :: t1 -> (
      match h1 with
      | [] -> ()
      | h2 :: t2 ->
          let key = t2 in
          let label = if h2 > 0 then Positive else Negative in
          Hashtbl.add table key label;
          populate_table table t1)

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

let to_list (table : t) =
  Hashtbl.fold (fun key label acc -> (key, label) :: acc) table []

let get_key (key : key) : int list = key
