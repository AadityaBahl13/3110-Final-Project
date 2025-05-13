open Finalproject.Data
open Finalproject.Lin_alg
open Finalproject.Perceptron
open Bogue
open Cairo
module W = Widget
module L = Layout

let width, height = (400, 400)
let canvas = W.sdl_area ~w:width ~h:height ()
let area = W.get_sdl_area canvas

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

(* ---------- Utilities ---------- *)
(* let transform (x, y) ~w ~h =
  let scale = 10 in
  let x' = (x * scale) + (w / 2) in
  let y' = (h / 2) - (y * scale) in
  (x', y') *)

let transform_f (x, y) ~w ~h =
  let xf = (float_of_int w /. 2.) +. (x *. 20.) in
  let yf = (float_of_int h /. 2.) -. (y *. 20.) in
  (int_of_float xf, int_of_float yf)

let get_data_bounds data =
  let xs, ys =
    List.fold_left
      (fun (xs, ys) (xv, _) ->
        match to_list xv |> List.hd with
        | [ x; y ] -> (x :: xs, y :: ys)
        | _ -> (xs, ys))
      ([], []) data
  in
  let min_x = List.fold_left min max_int xs in
  let max_x = List.fold_left max min_int xs in
  let min_y = List.fold_left min max_int ys in
  let max_y = List.fold_left max min_int ys in
  ( float_of_int min_x,
    float_of_int max_x,
    float_of_int min_y,
    float_of_int max_y )

let create_transform ~w ~h data =
  let min_x, max_x, min_y, max_y = get_data_bounds data in
  let margin = 50. in
  let data_width = max_x -. min_x in
  let data_height = max_y -. min_y in
  let scale_x = (float_of_int w -. (2. *. margin)) /. data_width in
  let scale_y = (float_of_int h -. (2. *. margin)) /. data_height in
  let scale = Float.min scale_x scale_y in
  let center_x = (min_x +. max_x) /. 2. in
  let center_y = (min_y +. max_y) /. 2. in
  fun (x, y) ->
    let xf = (float_of_int w /. 2.) +. ((x -. center_x) *. scale) in
    let yf = (float_of_int h /. 2.) -. ((y -. center_y) *. scale) in
    (int_of_float xf, int_of_float yf)

let draw_axes renderer ~w ~h ~data =
  let transform = create_transform ~w ~h data in
  let min_x, max_x, min_y, max_y = get_data_bounds data in
  let axis_color = Draw.opaque Draw.black in

  (* Draw x-axis from (min_x, 0) to (max_x, 0) *)
  let x0, y0 = transform (min_x, 0.) in
  let x1, y1 = transform (max_x, 0.) in
  Draw.line ~color:axis_color ~x0 ~y0 ~x1 ~y1 renderer;

  (* Draw y-axis from (0, min_y) to (0, max_y) *)
  let x0, y0 = transform (0., min_y) in
  let x1, y1 = transform (0., max_y) in
  Draw.line ~color:axis_color ~x0 ~y0 ~x1 ~y1 renderer

let make_plot_area (table : (tensor * label) list) =
  let width, height = (400, 400) in

  let widget = Widget.sdl_area ~w:width ~h:height () in

  try
    let area = Widget.get_sdl_area widget in
    let draw_fun renderer =
      (* draw_axes width height renderer; *)
      List.iter
        (fun (key, label_value) ->
          match to_list key |> List.hd with
          | x :: y :: _ ->
              let x', y' =
                transform_f (float_of_int x, float_of_int y) ~w:width ~h:height
              in
              let color = Draw.set_alpha 255 (color_of_label label_value) in
              Draw.circle ~color ~radius:5 ~x:x' ~y:y' renderer
          | _ -> ())
        table
    in
    Sdl_area.add area draw_fun;
    Layout.resident widget
  with Invalid_argument msg -> L.empty ~w:width ~h:height ()

(* ---------- App State ---------- *)

type screen =
  | Title
  | MainMenu
  | Train

let current_screen = ref Title
let previous_screen = ref None
let current_data = ref ([] : (tensor * label) list)
let current_table = ref init_data

(* --------- Perceptron --------- *)
(* let draw_points renderer =
  draw_axes width height renderer;
  List.iter
    (fun (xv, label) ->
      match to_list xv |> List.hd with
      | [ x; y ] ->
          let x', y' =
            transform_f (float_of_int x, float_of_int y) ~w:width ~h:height
          in
          let color = Draw.opaque (color_of_label label) in
          Draw.circle ~color ~radius:4 ~x:x' ~y:y' renderer
      | _ -> ())
    !current_data *)

let draw_points renderer =
  draw_axes ~w:width ~h:height ~data:!current_data renderer;
  let transform = create_transform ~w:width ~h:height !current_data in
  List.iter
    (fun (xv, label) ->
      match to_list xv |> List.hd with
      | [ x; y ] ->
          let x', y' = transform (float_of_int x, float_of_int y) in
          let color = Draw.opaque (color_of_label label) in
          Draw.circle ~color ~radius:4 ~x:x' ~y:y' renderer
      | _ -> ())
    !current_data;
  let cx, cy = (width / 2, height / 2) in
  Draw.circle ~color:(Draw.opaque Draw.green) ~radius:3 ~x:cx ~y:cy renderer

let compute_decision_boundary_points w1 w2 b min_x max_x =
  let y x = -.((w1 *. x) +. b) /. w2 in
  let x1 = min_x in
  let y1 = y x1 in
  let x2 = max_x in
  let y2 = y x2 in
  ((x1, y1), (x2, y2))

let draw_decision_boundary weights bias renderer =
  let b = float_of_int bias in
  let data = !current_data in
  match weights with
  | [ w1; w2 ] ->
      let min_x, max_x, _, _ = get_data_bounds data in
      let (x1, y1), (x2, y2) =
        compute_decision_boundary_points w1 w2 b min_x max_x
      in
      let transform = create_transform ~w:width ~h:height data in
      let px1, py1 = transform (x1, y1) in
      let px2, py2 = transform (x2, y2) in

      let color = Draw.opaque Draw.green in
      Draw.line ~color ~x0:px1 ~y0:py1 ~x1:px2 ~y1:py2 renderer
  | _ -> ()

let update_canvas ?weights bias area () =
  Sdl_area.clear area;
  Sdl_area.add area draw_points;
  Option.iter
    (fun w -> Sdl_area.add area (draw_decision_boundary w bias))
    weights

let run_training_stepwise steps_label =
  let perceptron = init !current_table 100 in
  let steps = ref 0 in
  let finished = ref false in

  let data_array = Array.of_list (data_to_list !current_table) in
  let idx = ref 0 in

  let rec step_graph () =
    if !finished then ignore (W.set_text steps_label "Done!")
    else if !idx >= Array.length data_array then finished := true
    else
      let input, label = data_array.(!idx) in
      incr idx;
      let updated = step perceptron input label in
      incr steps;
      W.set_text steps_label ("Steps: " ^ string_of_int !steps);
      if not updated then (
        let weights =
          to_list (get_weight perceptron) |> List.hd |> List.map float_of_int
        in
        List.iteri
          (fun i w ->
            print_endline
              ("Weight(step) " ^ string_of_int i ^ ": " ^ string_of_float w))
          weights;
        update_canvas ~weights (get_bias perceptron) area ());
      ignore (Timeout.add 5 step_graph)
  in
  ignore (Timeout.add 2 step_graph)

let run_training_final steps_label =
  let perceptron = init !current_table 100 in
  train perceptron;
  let weights =
    let w = to_list (get_weight perceptron) in
    match w with
    | [] -> []
    | hd :: _ ->
        List.iteri
          (fun i w ->
            print_endline
              ("Weight(final) " ^ string_of_int i ^ ": " ^ string_of_int w))
          hd;
        List.map float_of_int
          hd (* Adjust this depending on your perceptron's weight structure *)
  in
  update_canvas ~weights (get_bias perceptron) area ();
  W.set_text steps_label "Final result"

let visualize_perceptron () =
  let steps_label = W.label "Steps: 0" in

  let btn_steps =
    W.button "Train Step-by-Step" ~action:(fun _ ->
        run_training_stepwise steps_label)
  in
  let btn_final =
    W.button "Train Fully" ~action:(fun _ -> run_training_final steps_label)
  in

  let btn_back =
    W.button "Back To Main" ~action:(fun _ -> current_screen := MainMenu)
  in

  Sdl_area.add area draw_points;

  let layout =
    L.tower
      [
        L.resident canvas;
        L.flat_of_w [ btn_steps; btn_final; btn_back ];
        L.resident steps_label;
      ]
  in
  layout

(* ---------- Screens ---------- *)

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
  if Array.length Sys.argv <> 2 then (
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <csv_file>");
    exit 1);

  let file = Sys.argv.(1) in
  let table = read_from_csv file in
  let table_list = data_to_list table in
  current_data := table_list;
  current_table := table;

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

(* let main2 () = if Array.length Sys.argv <> 2 then ( print_endline ("Usage: "
   ^ Sys.argv.(0) ^ " <csv_file>"); exit 1); current_board := Bogue.of_layout
   (make_title_screen ()); Bogue.run !current_board

   (* let file = Sys.argv.(1) in try (* let table = read_from_csv file in *) (*
   let table_list = data_to_list table in *) (* let layout = make_plot_area
   table_list in *) let _ = change_screen (make_title_screen ()) in (* let
   title_layout = make_title_screen () in *) let board = Bogue.of_layout root in
   Bogue.run ~after_display:(fun () -> change_screen (make_title_screen ()))
   board with | Failure msg -> () | _ -> () *)

   let () = main (); main2 () *)
