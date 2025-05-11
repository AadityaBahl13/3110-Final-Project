open Finalproject.Data
open Finalproject.Lin_alg
open Bogue
open Cairo
module W = Widget
module L = Layout

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

(* let () =
  let hello = Widget.label "Hello world" in
  let image = Widget.image "data/greekfrog.jpg" in
  let layout = Layout.tower_of_w [ hello; image ] in
  let board = Bogue.of_layout layout in
  Bogue.run board *)

(* Transform feature space into screen space *)
let transform (x, y) ~w ~h =
  let scale = 10 in
  let x' = (x * scale) + (w / 2) in
  let y' = (h / 2) - (y * scale) in
  (x', y')

let draw_axes w h renderer =
  let axis_color = Draw.opaque Draw.black in
  Draw.line ~color:axis_color ~x0:0 ~y0:(h / 2) ~x1:w ~y1:(h / 2) renderer;
  Draw.line ~color:axis_color ~x0:(w / 2) ~y0:0 ~x1:(w / 2) ~y1:h renderer

let make_plot_area (table : (tensor * label) list) =
  let width, height = (400, 400) in

  let widget = Widget.sdl_area ~w:width ~h:height () in

  try
    let area = Widget.get_sdl_area widget in
    let draw_fun renderer =
      draw_axes width height renderer;

      List.iter
        (fun (key, label_value) ->
          match to_list key |> List.hd with
          | x :: y :: _ ->
              let x', y' = transform (x, y) ~w:width ~h:height in
              let color = Draw.set_alpha 255 (color_of_label label_value) in
              Draw.circle ~color ~radius:5 ~x:x' ~y:y' renderer
          | _ -> ())
        table
    in
    Sdl_area.add area draw_fun;
    Layout.resident widget
  with Invalid_argument msg -> L.empty ~w:width ~h:height ()

let main2 () =
  if Array.length Sys.argv <> 2 then (
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <csv_file>");
    exit 1);

  let file = Sys.argv.(1) in
  try
    let table = read_from_csv file in
    let table_list = data_to_list table in
    let layout = make_plot_area table_list in
    let board = Bogue.of_layout layout in
    Bogue.run board
  with
  | Failure msg -> ()
  | _ -> ()

let () =
  main ();
  main2 ()
