open Finalproject.Data
open Finalproject.Lin_alg
open Finalproject.Perceptron
open Finalproject.Decision_tree
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
  | DT

let current_screen = ref Title
let previous_screen = ref None
let current_data = ref ([] : (tensor * label) list)
let current_table = ref init_data
let step_idx = ref 0
let current_perceptron = ref None
let steps_taken = ref 0
let step_finished = ref false
let step_amount = ref 1

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
  if w2 = 0. then
    let x = -.b /. w1 in
    ((x, min_x), (x, max_x))
  else
    let y x = -.((w1 *. x) +. b) /. w2 in
    let x1 = min_x in
    let y1 = y x1 in
    let x2 = max_x in
    let y2 = y x2 in
    ((x1, y1), (x2, y2))

let draw_weight_vector w1 w2 renderer =
  (* Draw weight vector as a line starting from origin (0, 0) *)
  let color = Draw.opaque Draw.red in
  let x0, y0 = (0., 0.) in
  (* starting point *)
  let x1, y1 = (w1, w2) in
  (* direction from origin *)

  let transform = create_transform ~w:width ~h:height !current_data in
  let px0, py0 = transform (x0, y0) in
  let px1, py1 = transform (x1, y1) in

  Draw.line ~color ~x0:px0 ~y0:py0 ~x1:px1 ~y1:py1 renderer

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
      Draw.line ~color ~x0:px1 ~y0:py1 ~x1:px2 ~y1:py2 renderer;
      draw_weight_vector w1 w2 renderer;
      Printf.printf "Drawing decision boundary from (%f, %f) to (%f, %f)\n" x1
        y1 x2 y2
  | _ -> ()

let rec draw_dt_helper (dt : Finalproject.Decision_tree.t) renderer trans
    x_stack y_stack scale =
  match dt with
  | Leaf y -> ()
  | Tree t -> begin
      let draw1, draw2, y1, y2 =
        match (get_left t, get_right t) with
        | Leaf y1, Leaf y2 -> (true, true, y1, y2)
        | Leaf y1, _ -> (true, false, y1, negative)
        | _, Leaf y2 -> (false, true, positive, y2)
        | _ -> (false, false, positive, negative)
      in
      if draw1 || draw2 then (
        let l_color =
          if y1 = positive then Draw.transp Draw.blue else Draw.transp Draw.red
        in
        let r_color =
          if y2 = positive then Draw.transp Draw.blue else Draw.transp Draw.red
        in
        let x_min, x_max = Stack.pop x_stack in
        let y_min, y_max = Stack.pop y_stack in
        let s_dim, s_val = get_split_dim_and_val t in
        let xl_min, yl_min, wl, hl, xr_min, yr_min, wr, hr =
          if s_dim = 0 then
            ( x_min,
              y_min,
              s_val - x_min,
              y_max - y_min,
              s_val,
              y_min,
              x_max - s_val,
              y_max - y_min )
          else
            ( x_min,
              y_min,
              x_max - x_min,
              s_val - y_min,
              x_min,
              s_val,
              x_max - x_min,
              y_max - s_val )
        in
        let xl_min_t, yl_min_t =
          trans (float_of_int xl_min, float_of_int yl_min)
        in
        let xr_min_t, yr_min_t =
          trans (float_of_int xr_min, float_of_int yr_min)
        in

        if draw1 then
          Draw.rectangle renderer ~color:l_color ~w:(wl * scale) ~h:(hl * scale)
            ~x:xl_min_t
            ~y:(yl_min_t - (hl * scale))
        else if s_dim = 0 then (
          Stack.push (x_min, s_val) x_stack;
          Stack.push (y_min, y_max) y_stack;
          draw_dt_helper (get_left t) renderer trans x_stack y_stack scale)
        else (
          Stack.push (x_min, x_max) x_stack;
          Stack.push (y_min, s_val) y_stack;
          draw_dt_helper (get_left t) renderer trans x_stack y_stack scale);

        if draw2 then
          Draw.rectangle renderer ~color:r_color ~w:(scale * wr) ~h:(scale * hr)
            ~x:xr_min_t
            ~y:(yr_min_t - (hr * scale))
        else if s_dim = 0 then (
          Stack.push (s_val, x_max) x_stack;
          Stack.push (y_min, y_max) y_stack;
          draw_dt_helper (get_right t) renderer trans x_stack y_stack scale)
        else (
          Stack.push (x_min, x_max) x_stack;
          Stack.push (s_val, y_max) y_stack;
          draw_dt_helper (get_right t) renderer trans x_stack y_stack scale))
      else
        let s_dim, s_val = get_split_dim_and_val t in
        let x_min, x_max = Stack.top x_stack in
        let y_min, y_max = Stack.top y_stack in
        if s_dim = 0 then (
          Stack.push (x_min, s_val) x_stack;
          Stack.push (y_min, y_max) y_stack;
          draw_dt_helper (get_left t) renderer trans x_stack y_stack scale;
          Stack.push (s_val, x_max) x_stack;
          Stack.push (y_min, y_max) y_stack;
          draw_dt_helper (get_right t) renderer trans x_stack y_stack scale)
        else (
          Stack.push (x_min, x_max) x_stack;
          Stack.push (y_min, s_val) y_stack;
          draw_dt_helper (get_left t) renderer trans x_stack y_stack scale;
          Stack.push (x_min, x_max) x_stack;
          Stack.push (s_val, y_max) y_stack;
          draw_dt_helper (get_right t) renderer trans x_stack y_stack scale)
    end

let draw_dt dt renderer =
  let transform = create_transform ~w:width ~h:height !current_data in
  let min_x, max_x, min_y, max_y = get_data_bounds !current_data in
  let data_width = max_x -. min_x in
  let data_height = max_y -. min_y in
  let margin = 50. in
  let scale_x = (float_of_int width -. (2. *. margin)) /. data_width in
  let scale_y = (float_of_int height -. (2. *. margin)) /. data_height in
  let scale = int_of_float (Float.min scale_x scale_y) in
  let x_bound_stack = Stack.create () in
  Stack.push (int_of_float min_x, int_of_float max_x) x_bound_stack;
  let y_bound_stack = Stack.create () in
  Stack.push (int_of_float min_y, int_of_float max_y) y_bound_stack;
  draw_dt_helper dt renderer transform x_bound_stack y_bound_stack scale

let update_canvas ?weights bias area () =
  Sdl_area.clear area;
  Sdl_area.add area draw_points;
  match weights with
  | None -> ()
  | Some w -> Sdl_area.add area (draw_decision_boundary w bias)
(* Option.iter (fun w -> Sdl_area.add area (draw_decision_boundary w bias))
   weights *)

let update_canvas_dt dt area () =
  Sdl_area.clear area;
  Sdl_area.add area draw_points;
  Sdl_area.add area (draw_dt dt)

let run_training_stepwise steps_label =
  let perceptron = init_perceptron !current_table 1000 in
  current_perceptron := Some perceptron;
  step_idx := 0;
  steps_taken := 0;
  step_finished := false;
  W.set_text steps_label "Steps: 0      ";
  update_canvas ~weights:([] : float list) 0 area () (* Clear previous line *)

(* Perform one training step manually *)
let do_step steps_label =
  match !current_perceptron with
  | None -> ()
  | Some perceptron ->
      let data_array = Array.of_list (data_to_list !current_table) in
      if !step_finished then W.set_text steps_label "Done!"
      else if !step_idx >= Array.length data_array then
        if check_perceptron perceptron then (
          step_finished := true;
          W.set_text steps_label "Done!")
        else step_idx := 0
      else
        let input, label = data_array.(!step_idx) in
        incr step_idx;
        incr steps_taken;
        let updated = step perceptron input label in
        let weights =
          to_list (get_weight perceptron) |> List.hd |> List.map float_of_int
        in
        W.set_text steps_label
          ("Steps: " ^ string_of_int !steps_taken ^ "      ");
        update_canvas ~weights (get_bias perceptron) area ();
        if not updated then
          List.iteri
            (fun i w ->
              print_endline
                ("Weight [" ^ string_of_int i ^ "]: " ^ string_of_float w))
            weights

let run_training_final steps_label =
  let perceptron = init_perceptron !current_table 100 in
  Finalproject.Perceptron.train perceptron;
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
        print_endline ("Bias: " ^ string_of_int (get_bias perceptron));
        List.map float_of_int
          hd (* Adjust this depending on your perceptron's weight structure *)
  in
  update_canvas ~weights (get_bias perceptron) area ();
  W.set_text steps_label "Final result"

let run_training_final_dt steps_label =
  let dt = init_decision_tree !current_table 50 in
  Finalproject.Decision_tree.train dt;
  update_canvas_dt dt area ();
  W.set_text steps_label "Final result"

let visualize_perceptron () =
  let steps_label = W.label "Steps: 0" in
  let btn_next = W.button "Next Step" ~action:(fun _ -> do_step steps_label) in
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
        L.flat_of_w ~sep:10 [ steps_label; btn_next ];
      ]
  in
  layout

let visualize_decision_tree () =
  let steps_label = W.label "Max Depth: 0" in

  let btn_final =
    W.button "Train" ~action:(fun _ -> run_training_final_dt steps_label)
  in

  let btn_back =
    W.button "Back To Main" ~action:(fun _ -> current_screen := MainMenu)
  in

  Sdl_area.add area draw_points;

  let layout =
    L.tower
      [
        L.resident canvas;
        L.flat_of_w [ btn_final; btn_back ];
        L.resident steps_label;
      ]
  in
  layout

(* ---------- Screens ---------- *)

let build_screen scr =
  match scr with
  | Title ->
      let title_label = W.label "Our Crack At Machine Learning (OCAML)" in
      let sub_title =
        W.label "Pattern Matchers: Aaditya Bahl, Danielle Imogu, Jonah Benard"
      in
      let start_btn =
        W.button "Start" ~action:(fun _ -> current_screen := MainMenu)
      in
      L.tower_of_w ~w:400 [ title_label; sub_title; start_btn ]
  | MainMenu ->
      let visualize_button =
        Widget.button "Visualize Perceptron" ~action:(fun _ ->
            current_screen := Train)
      in
      let read_data_button =
        Widget.button "Visualize Decision Tree" ~action:(fun _ ->
            current_screen := DT)
      in
      let back_button =
        Widget.button "Back" ~action:(fun _ -> current_screen := Title)
      in
      let label = W.label "Main Menu" in
      Layout.tower_of_w
        [ label; visualize_button; read_data_button; back_button ]
  | Train -> visualize_perceptron ()
  (* if Sys.argv.(2) = "perceptron" then visualize_perceptron () else
     visualize_decision_tree () *)
  | DT -> visualize_decision_tree ()
(* | _ -> L.empty ~w:400 ~h:400 () *)

(* ---------- Layout + Update ---------- *)

let root_layout = L.empty ~w:400 ~h:400 ~name:"Our Crack At Machine Learning" ()

let update_screen () =
  if Some !current_screen <> !previous_screen then (
    let new_layout = build_screen !current_screen in
    L.set_rooms ~sync:false root_layout [ new_layout ];
    L.fit_content root_layout;
    previous_screen := Some !current_screen)

(* ---------- Entry Point ---------- *)

let string_to_tensor str =
  let int_list = List.map int_of_string (String.split_on_char ' ' str) in
  Finalproject.Lin_alg.create [ int_list ]

let rec main_loop_perceptron perceptron =
  try
    print_endline "Please enter a new point:";
    let new_vec = input_line stdin in
    if new_vec = "exit" then exit 0;
    let tensor = string_to_tensor new_vec in
    let prediction = Finalproject.Perceptron.predict perceptron tensor in
    print_endline
      ("The perceptron predicts that this point has label "
      ^ label_to_string (tensor, prediction));
    print_newline ();
    main_loop_perceptron perceptron
  with
  | Finalproject.Lin_alg.InvalidDimensions ->
      print_endline
        ("The new point has to be of dimension "
        ^ string_of_int (get_dimension !current_table));
      print_newline ();
      main_loop_perceptron perceptron
  | int_of_string ->
      print_endline "This is not a valid vector";
      print_newline ();
      main_loop_perceptron perceptron

let rec main_loop_decision_tree tree =
  try
    print_endline "Please enter a new point:";
    let new_vec = input_line stdin in
    if new_vec = "exit" then exit 0;
    let tensor = string_to_tensor new_vec in
    let prediction = Finalproject.Decision_tree.predict tree tensor in
    print_endline
      ("The decision tree predicts that this point has label "
      ^ label_to_string (tensor, prediction));
    print_newline ();
    main_loop_decision_tree tree
  with
  | Finalproject.Lin_alg.InvalidDimensions ->
      print_endline
        ("The new point has to be of dimension "
        ^ string_of_int (get_dimension !current_table));
      print_newline ();
      main_loop_decision_tree tree
  | int_of_string ->
      print_endline "This is not a valid vector";
      print_newline ();
      main_loop_decision_tree tree

let () =
  try
    if Array.length Sys.argv < 4 || Array.length Sys.argv > 5 then (
      print_endline
        ("Usage: " ^ Sys.argv.(0)
       ^ " <csv_file> <model type> <model param> <g (gui)>");
      exit 1);

    if int_of_string Sys.argv.(3) <= 0 then
      failwith "You must pass a positive model parameter";

    let file = Sys.argv.(1) in
    let table = read_from_csv file in
    let table_list = data_to_list table in
    current_data := table_list;
    current_table := table;

    if Array.length Sys.argv = 5 then
      if Sys.argv.(4) <> "g" then failwith "Invalid argument"
      else if get_dimension !current_table <> 2 then
        failwith "Cannot provide GUI visualization of non 2D data";

    (* Set initial layout *)
    if Array.length Sys.argv = 5 then (
      let init = build_screen !current_screen in
      L.set_rooms ~sync:false root_layout [ init ];
      L.fit_content root_layout;
      Bogue.run ~before_display:update_screen (Bogue.of_layout root_layout));

    if Sys.argv.(2) = "perceptron" then (
      let perceptron =
        init_perceptron !current_table (int_of_string Sys.argv.(3))
      in
      Finalproject.Perceptron.train perceptron;
      main_loop_perceptron perceptron)
    else if Sys.argv.(2) = "decision_tree" then (
      let tree =
        init_decision_tree !current_table (int_of_string Sys.argv.(3))
      in
      Finalproject.Decision_tree.train tree;
      main_loop_decision_tree tree)
    else
      failwith
        "Invalid model type. You must pass \"perceptron\" or \"decision_tree\" \
         as your model type"
  with
  | Failure x -> print_endline ("Error: " ^ x)
  | _ -> print_endline "Error: An unknown exception has occured"

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
