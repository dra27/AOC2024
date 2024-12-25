module Array = struct
  include Array

  let init_right_fold l f acc =
    if l = 0 then acc, [||] else
    if l < 0 then invalid_arg "Array.init_fold"
    else
     let acc, res = f acc (pred l) in
     let acc = ref acc in
     let res = make l res in
     for i = (l - 2) downto 0 do
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

type cell = F | O of bool | B

let parse =
  let rec loop warehouse = function
  | [] -> assert false
  | "" :: lines ->
      let gather line acc =
        let gather c acc = direction_of_char c :: acc in
        String.fold_right gather line acc in
      warehouse, List.fold_right gather lines []
  | line :: lines ->
      loop (line::warehouse) lines in
  loop []

let create_warehouse double warehouse =
  let double = Bool.to_int double in
  let size = String.length (List.hd warehouse) in
  let gather (robot, warehouse) y =
    match warehouse with
    | [] -> assert false
    | line :: lines ->
        let gather acc x =
          let c = line.[x lsr double] in
          match c with
          | '@' ->
              if double = 0 || x land 1 = 0 then
                (x, y), F
              else
                (acc, F)
          | '#' ->
              acc, B
          | '.' ->
              acc, F
          | 'O' ->
              acc, O (double = 0 || x land 1 = 0)
          | _ ->
              assert false in
        let robot, row = Array.init_right_fold (size lsl double) gather robot in
        (robot, lines), row in
  let (robot, lines), warehouse =
    Array.init_right_fold size gather ((-1, -1), warehouse) in
  assert (lines = []);
  (robot, warehouse)

let part double (warehouse, route) =
  let robot, warehouse = create_warehouse double warehouse in
  let execute ((x, y) as current) command =
    let dx, dy, double =
      match command with
      | N -> Fun.id, pred, double
      | E -> succ, Fun.id, false
      | S -> Fun.id, succ, double
      | W -> pred, Fun.id, false in
    let (x', y') as next = dx x, dy y in
    match warehouse.(y').(x') with
    | F -> next
    | O i ->
        let write x y v = warehouse.(y).(x) <- v; true in
        let scan _ _ _ = true in
        let rec shift write v x y =
          match warehouse.(y).(x) with
          | F ->
              write x y v
          | (O is_left) as v' ->
              if double then
                if shift write warehouse.(y).(x) x (dy y) then
                  let _ = write x y v in
                  if is_left then
                    if shift write warehouse.(y).(x + 1) (x + 1) (dy y) then
                      write (x + 1) y F
                    else
                      false
                  else if shift write warehouse.(y).(x - 1) (x - 1) (dy y) then
                    write (x - 1) y F
                  else
                    false
                else
                  false
              else if shift write v' (dx x) (dy y) then
                write x y v
              else
                false
          | B ->
              false in
        if shift scan F x' y' && shift write F x' y' then
          next
        else
         current
    | B -> current in
  let gather acc y row =
    let gather acc x = function
    | F | B | O false -> acc
    | O true -> 100 * y + x + acc in
    Array.foldi_left gather acc row in
  let _ = List.fold_left execute robot route in
  Array.foldi_left gather 0 warehouse

let part1 = part false
let part2 = part true

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
                 Day 15; Puzzle 1 = %d\n\
                 Day 15; Puzzle 2; test = %d\n\
                 Day 15; Puzzle 2 = %d\n" (part1 test) (part1 input)
                                          (part2 test) (part2 input)
