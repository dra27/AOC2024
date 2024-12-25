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

  let foldi_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r i (unsafe_get a i)
    done;
    !r
end

type direction = N | E | S | W

let direction_of_char = function
| '^' -> N
| '>' -> E
| 'v' -> S
| '<' -> W
| _ -> invalid_arg "direction_of_char"

type cell = F | O | B

let cell_of_char = function
| '.' -> F(*loor*)
| 'O' -> O(*bstacle*)
| '#' -> B(*oundary*)
| _ -> invalid_arg "cell_of_char"

let parse input =
  match input with
  | [] -> assert false
  | line :: _ ->
      let size = String.length line in
      let gather (robot, input) y =
        match input with
        | [] -> assert false
        | line :: lines ->
            let gather acc x =
              let c = line.[x] in
              if c = '@' then
                (x, y), F
              else
                acc, cell_of_char c in
            let robot, row = Array.init_fold size gather robot in
            (robot, lines), row in
      match Array.init_fold size gather ((-1, -1), input) with
      | (robot, "" :: lines), warehouse ->
          let gather line acc =
            let gather c acc = direction_of_char c :: acc in
            String.fold_right gather line acc in
          robot, warehouse, List.fold_right gather lines []
      | _, _ -> assert false

let part1 (robot, warehouse, route) =
  let execute ((x, y) as current) command =
    let dx, dy =
      match command with
      | N -> Fun.id, pred
      | E -> succ, Fun.id
      | S -> Fun.id, succ
      | W -> pred, Fun.id in
    let (x', y') as next = dx x, dy y in
    match warehouse.(y').(x') with
    | F -> next
    | O ->
        let rec shift x y =
          match warehouse.(y).(x) with
          | F ->
              warehouse.(y).(x) <- O;
              warehouse.(y').(x') <- F;
              next
          | O ->
              shift (dx x) (dy y)
          | B ->
              current in
        shift x' y'
    | B -> current in
  let gather acc y row =
    let gather acc x = function
    | F | B -> acc
    | O -> 100 * y + x + acc in
    Array.foldi_left gather acc row in
  let _ = List.fold_left execute robot route in
  Array.foldi_left gather 0 warehouse

let test =
  parse @@ String.split_on_char '\n' (String.trim {|
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
|})

let input =
  In_channel.with_open_text "input-15" @@ fun ic ->
    parse (In_channel.input_lines ic)

let () =
  Printf.printf "Day 15; Puzzle 1; test = %d\n\
                 Day 15; Puzzle 1 = %d\n" (part1 test) (part1 input)
