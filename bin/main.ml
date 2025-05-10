open Finalproject.Data
open Finalproject.Lin_alg

let string_of_list lst =
  List.fold_left (fun acc e -> acc ^ " " ^ string_of_int e) "[" lst ^ " ]"

let main () =
  if Array.length Sys.argv <> 2 then (
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <csv_file>");
    exit 1);

  let file = Sys.argv.(1) in
  try
    let table = read_from_csv file in
    let table_list = data_to_list table in

    (* Print the table contents using label_to_string *)
    print_endline "Table Contents:";
    List.iter
      (fun (key, label_value) ->
        let key_str =
          String.concat ", " (List.map string_of_list (to_list key))
        in
        let label_str = label_to_string (key, label_value) in
        print_endline ("[" ^ key_str ^ "] -> " ^ label_str))
      table_list;

    (* Print label counts *)
    let pos_count, neg_count = count_labels table in
    print_endline ("Positive Labels: " ^ string_of_int pos_count);
    print_endline ("Negative Labels: " ^ string_of_int neg_count)
  with
  | Failure msg -> print_endline ("Error: " ^ msg)
  | _ -> print_endline "An unknown error occurred."

let () = main ()
