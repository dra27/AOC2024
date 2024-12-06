module Array = struct
  include Array

  let init_fold l f acc =
    if l = 0 then acc, [||] else
    if l < 0 then invalid_arg "Array.init_fold"
    else
     let acc, res = f acc 0 in
     let acc = ref acc in
     let res = make l res in
     for i = 1 to pred l do
       let acc', elt = f !acc i in
       acc := acc';
       unsafe_set res i elt
     done;
     !acc, res
end

type cell = Empty | Seen | Obstacle

let parse lines =
  let start = ref None in
  let init_rows rows y =
    match rows with
    | row::rows ->
        let init_row x =
          match row.[x] with
          | '.' -> Empty
          | '#' -> Obstacle
          | '^' -> start := Some (x, y); Seen
          | _ -> assert false in
        rows, Array.init (String.length row) init_row
    | [] -> assert false in
  let _, lab = Array.init_fold (List.length lines) init_rows lines in
  lab, Option.get !start

let test =
  parse @@ String.split_on_char '\n' (String.trim {|
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
|})

let input =
  In_channel.with_open_text "input-06" @@ fun ic ->
    parse @@ In_channel.input_lines ic

type deja_vu = N | E | S | W

let turn_right = function
| N -> E
| E -> S
| S -> W
| W -> N

let part1 (lab, (x, y)) =
  let max_x = Array.length lab.(0) in
  let max_y = Array.length lab in
  let rec loop count x y direction =
    let shift incr decr value =
      if direction = incr then
        succ value
      else if direction = decr then
        pred value
      else
        value in
    let x' = shift E W x in
    let y' = shift S N y in
    if x' = -1 || y' = -1 || x' = max_x || y' = max_y then
      count
    else
      match lab.(y').(x') with
      | Seen ->
          loop count x' y' direction
      | Obstacle ->
          loop count x y (turn_right direction)
      | Empty ->
          lab.(y').(x') <- Seen;
          loop (succ count) x' y' direction in
  loop 1 x y N

let () =
  Printf.printf "Day 6; Puzzle 1; test = %d\n\
                 Day 6; Puzzle 1 = %d\n" (part1 test) (part1 input)
