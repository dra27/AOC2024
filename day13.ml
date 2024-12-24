let parse folder input =
  let gather ((phase, machines) as acc) line =
    match phase with
    | `Base when line = "" -> acc
    | `Base ->
        Scanf.sscanf line "Button A: X+%u, Y+%u"
          @@ fun x y -> `A (x, y), machines
    | `A (x, y) ->
        Scanf.sscanf line "Button B: X+%u, Y+%u"
          @@ fun x' y' -> `B (x, y, x', y'), machines
    | `B (x, y, x', y') ->
        Scanf.sscanf line "Prize: X=%u, Y=%u"
          @@ fun x'' y'' -> (`Base, ((x, x', x''), (y, y', y''))::machines) in
  let phase, machines = folder gather (`Base, []) input in
  assert (phase = `Base);
  machines

let part offset acc ((ax, bx, rx), (ay, by, ry)) =
  let rx = offset + rx in
  let ry = offset + ry in
  let prod = ax * ay in
  let b_numer = rx * (prod / ax) - ry * (prod / ay) in
  let b_denom = bx * (prod / ax) - by * (prod / ay) in
  if b_numer mod b_denom = 0 then
    let b = b_numer / b_denom in
    let a_numer = (rx - bx * b) in
    if a_numer mod ax = 0 then
      let a = (rx - bx * b) / ax in
      assert (a * ax + b * bx = rx && a * ay + b * by = ry);
      3 * a + b + acc
    else
      acc
  else
    acc

let part1 = List.fold_left (part 0) 0
let part2 = List.fold_left (part 10000000000000) 0

let test =
  parse List.fold_left @@ String.split_on_char '\n' (String.trim {|
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
|})

let input =
  In_channel.with_open_text "input-13" @@ parse In_channel.fold_lines

let () =
  Printf.printf "Day 13; Puzzle 1; test = %d\n\
                 Day 13; Puzzle 1 = %d\n\
                 Day 13; Puzzle 2; test = %d\n\
                 Day 13; Puzzle 2 = %d\n" (part1 test) (part1 input)
                                          (part2 test) (part2 input)
