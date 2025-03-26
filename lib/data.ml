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
      | h2 :: t2 -> Hashtbl.add table t2 (if h2 > 0 then Positive else Negative)
      )

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
