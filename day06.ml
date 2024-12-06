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
  String.split_on_char '\n' (String.trim {|
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
  In_channel.with_open_text "input-06" In_channel.input_lines

type deja_vu = N | E | S | W

let turn_right = function
| N -> E
| E -> S
| S -> W
| W -> N

let shift max_x max_y x y direction =
  let shift incr decr value =
    if direction = incr then
      succ value
    else if direction = decr then
      pred value
    else
      value in
  let x = shift E W x in
  let y = shift S N y in
  x, y, (x = -1 || y = -1 || x = max_x || y = max_y)

let part1 input =
  let (lab, (x, y)) = parse input in
  let shift = shift (Array.length lab.(0)) (Array.length lab) in
  let rec loop count x y direction =
    let x', y', escapes = shift x y direction in
    if escapes then
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

module PathSet = Set.Make(struct
  type t = int * int * deja_vu
  let compare = compare
end)

let part2 input =
  let (lab, (x, y)) = parse input in
  let shift = shift (Array.length lab.(0)) (Array.length lab) in
  let rec loops path x y direction =
    let x', y', escapes = shift x y direction in
    if escapes then
      false
    else
      match lab.(y').(x') with
      | Obstacle ->
          loops path x y (turn_right direction)
      | Empty | Seen ->
          let this = (x', y', direction) in
          if PathSet.mem this path then
            true
          else
            loops (PathSet.add this path) x' y' direction in
  let rec loop count path x y direction =
    let x', y', escapes = shift x y direction in
    if escapes then
      count
    else
      match lab.(y').(x') with
      | Obstacle ->
          loop count path x y (turn_right direction)
      | Seen ->
          let path = PathSet.add (x', y', direction) path in
          loop count path x' y' direction
      | Empty ->
          lab.(y').(x') <- Obstacle;
          let count =
            if loops path x y (turn_right direction) then
              succ count
            else
              count in
          lab.(y').(x') <- Seen;
          let path = PathSet.add (x', y', direction) path in
          loop count path x' y' direction in
  loop 0 (PathSet.add (x, y, N) PathSet.empty) x y N

let () =
  Printf.printf "Day 6; Puzzle 1; test = %d\n\
                 Day 6; Puzzle 1 = %d\n\
                 Day 6; Puzzle 2; test = %d\n\
                 Day 6; Puzzle 2 = %d\n" (part1 test) (part1 input)
                                         (part2 test) (part2 input)
