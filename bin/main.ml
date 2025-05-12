open Finalproject.Data
open Finalproject.Lin_alg
open Finalproject.Perceptron
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

(* ---------- Utilities ---------- *)
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

(* ---------- App State ---------- *)

type screen = Title | MainMenu | Train

let current_screen = ref Title
let previous_screen = ref None
let current_data = ref ([] : (tensor * label) list)

(* ---------- Screens ---------- *)

let visualize_perceptron () =
  let width, height = (400, 400) in
  let widget = W.sdl_area ~w:width ~h:height () in
  let area = W.get_sdl_area widget in

  let draw_points renderer =
    draw_axes width height renderer;
    List.iter
      (fun (xv, label) ->
        match to_list xv |> List.hd with
        | [ x; y ] ->
            let x', y' = transform (x, y) ~w:width ~h:height in
            let color = Draw.opaque (color_of_label label) in
            Draw.circle ~color ~radius:4 ~x:x' ~y:y' renderer
        | _ -> ())
      !current_data
  in
  Sdl_area.add area draw_points;

  let step_counter = ref 0 in
  let steps_label = W.label ("Steps: " ^ string_of_int !step_counter) in

  (* let update_plot_and_boundary weights =
    let draw_boundary renderer =
      match weights with
      | w0 :: w1 :: w2 :: _ ->
          let x1 = -20. in
          let x2 = 20. in
          let y1 = -.(w0 +. (w1 *. x1)) /. w2 in
          let y2 = -.(w0 +. (w1 *. x2)) /. w2 in
          let x1', y1' =
            transform (int_of_float x1, int_of_float y1) ~w:width ~h:height
          in
          let x2', y2' =
            transform (int_of_float x2, int_of_float y2) ~w:width ~h:height
          in
          let color = Draw.opaque Draw.green in
          Draw.line ~color ~x0:x1' ~y0:y1' ~x1:x2' ~y1:y2' renderer
      | _ -> ()
    in
    Sdl_area.clear area;
    Sdl_area.add area draw_points;
    Sdl_area.add area draw_boundary
  in *)
  let train_button =
    W.button "Start Training" ~action:(fun _ -> print_endline "Start")
  in
  L.tower_of_w [ widget; train_button; steps_label ]

let build_screen scr =
  match scr with
  | Title ->
      let title_label = W.label "OCAML Final Project" in
      let start_btn =
        W.button "Start" ~action:(fun _ -> current_screen := MainMenu)
      in
      L.tower_of_w ~w:400 [ title_label; start_btn ]
  | MainMenu ->
      let visualize_button =
        Widget.button "Visualize Perceptron" ~action:(fun _ ->
            current_screen := Train)
      in
      let read_data_button =
        Widget.button "Read Data" ~action:(fun _ -> print_endline "Read Data")
      in
      let back_button =
        Widget.button "Back" ~action:(fun _ -> current_screen := Title)
      in
      let label = W.label "Main Menu" in
      Layout.tower_of_w
        [ label; visualize_button; read_data_button; back_button ]
  | Train -> visualize_perceptron ()
(* | _ -> L.empty ~w:400 ~h:400 () *)

(* ---------- Layout + Update ---------- *)

let root_layout = L.empty ~w:400 ~h:400 ~name:"Main Layout" ()

let update_screen () =
  if Some !current_screen <> !previous_screen then (
    let new_layout = build_screen !current_screen in
    L.set_rooms ~sync:false root_layout [ new_layout ];
    L.fit_content root_layout;
    previous_screen := Some !current_screen)

(* ---------- Entry Point ---------- *)

let () =
  (* Set initial layout *)
  let init = build_screen !current_screen in
  L.set_rooms ~sync:false root_layout [ init ];
  L.fit_content root_layout;
  Bogue.run ~before_display:update_screen (Bogue.of_layout root_layout)

(* ---------- Other Functions ---------- *)
let main_menu () =
  let visualize_button =
    Widget.button "Visualize Perceptron" ~action:(fun _ ->
        print_endline "Perceptron")
  in
  let read_data_button =
    Widget.button "Read Data" ~action:(fun _ -> print_endline "Read Data")
  in
  Layout.tower_of_w [ visualize_button; read_data_button ]

let make_title_screen () =
  let title_label = W.label "OCAML Final Project" in
  let start_btn = W.button "Start" ~action:(fun x -> print_endline "Next") in
  let inner_layout = L.tower_of_w ~w:400 [ title_label; start_btn ] in
  L.set_height inner_layout 400;
  inner_layout

(* 
let main2 () =
  if Array.length Sys.argv <> 2 then (
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <csv_file>");
    exit 1);
  current_board := Bogue.of_layout (make_title_screen ());
  Bogue.run !current_board

(* let file = Sys.argv.(1) in
  try
    (* let table = read_from_csv file in *)
    (* let table_list = data_to_list table in *)
    (* let layout = make_plot_area table_list in *)
    let _ = change_screen (make_title_screen ()) in
    (* let title_layout = make_title_screen () in *)
    let board = Bogue.of_layout root in
    Bogue.run
      ~after_display:(fun () -> change_screen (make_title_screen ()))
      board
  with
  | Failure msg -> ()
  | _ -> () *)

let () =
  main ();
  main2 () *)
