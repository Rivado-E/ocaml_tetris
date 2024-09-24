[@@@ocaml.warning "-33-32-37-27"]

module Color = struct
  let darkGrey = Raylib.Color.create 26 31 40 255
  let green = Raylib.Color.create 47 230 23 255
  let red = Raylib.Color.create 232 18 18 255
  let orange = Raylib.Color.create 226 116 17 255
  let yellow = Raylib.Color.create 237 234 4 255
  let purple = Raylib.Color.create 166 0 247 255
  let cyan = Raylib.Color.create 21 204 209 255
  let blue = Raylib.Color.create 13 64 216 255
  let lightBlue = Raylib.Color.create 59 85 162 255
  let darkBlue = Raylib.Color.create 44 44 127 255
end

module Grid = struct
  type cell =
    | Empty
    | Iblock
    | Oblock
    | Jblock
    | Lblock
    | Sblock
    | Zblock
    | Tblock

  type t =
    { rows : int
    ; cols : int
    ; cells : cell array array
    }

  let create rows cols =
    let cells = Array.make_matrix rows cols Empty in
    { rows; cols; cells }
  ;;

  let set_cell grid row col value =
    if row >= 0 && row < grid.rows && col >= 0 && col < grid.cols
    then grid.cells.(row).(col) <- value
  ;;

  let get_cell grid row col =
    if row >= 0 && row < grid.rows && col >= 0 && col < grid.cols
    then Some grid.cells.(row).(col)
    else None
  ;;

  let cell_to_color = function
    | Empty -> Color.darkGrey
    | Iblock -> Color.blue
    | Oblock -> Color.yellow
    | Jblock -> Color.darkBlue
    | Lblock -> Color.orange
    | Sblock -> Color.green
    | Zblock -> Color.red
    | Tblock -> Color.purple
  ;;

  let cell_to_number = function
    | Empty -> 0
    | Iblock -> 1
    | Oblock -> 2
    | Jblock -> 3
    | Lblock -> 4
    | Sblock -> 5
    | Zblock -> 6
    | Tblock -> 7
  ;;

  let print grid =
    Array.iter
      (fun row ->
        Array.iter (fun x -> x |> cell_to_number |> Printf.printf "%d ") row;
        print_endline "")
      grid.cells
  ;;

  let draw grid =
    let cellSize = 30 in
    Array.iteri
      (fun r row ->
        Array.iteri
          (fun c cell ->
            let pos_x = (c * cellSize) + 11 in
            let pos_y = (r * cellSize) + 11 in
            let width = cellSize - 1 in
            let height = cellSize - 1 in
            let color = cell_to_color cell in
            Raylib.draw_rectangle pos_x pos_y width height color)
          row)
      grid.cells
  ;;
end

let setup () =
  Raylib.init_window 320 900 "raylib tetris";
  Raylib.set_target_fps 60
;;

let rec loop () =
  let g = Grid.create 20 10 in
  if Raylib.window_should_close ()
  then Raylib.close_window ()
  else Raylib.begin_drawing ();
  (* Grid.print g; *)
  Grid.draw g;
  Raylib.clear_background Color.darkBlue;
  Raylib.end_drawing ();
  loop ()
;;

let () = setup () |> loop
