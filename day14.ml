let parse folder =
  let gather acc line =
    Scanf.sscanf line "p=%u,%u v=%d,%d"
      @@ fun x y vx vy -> ((x, y), (vx, vy))::acc in
  folder gather []

let test =
  11, 7, parse List.fold_left @@ String.split_on_char '\n' (String.trim {|
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|})

let input =
  101, 103, In_channel.with_open_text "input-14" @@ parse In_channel.fold_lines

let part1 (width, height, robots) =
  let mid_width = (width - 1) lsr 1 in
  let mid_height = (height - 1) lsr 1 in
  let move_and_count n ((q1, q2, q3, q4) as acc) ((x, y), (vx, vy)) =
    let wrap v m = let v = v mod m in if v < 0 then v + m else v in
    let x, y = wrap (x + n * vx) width, wrap (y + n * vy) height in
    if x = mid_width || y = mid_height then
      acc
    else
      if x < mid_width then
        if y < mid_height then
          (succ q1, q2, q3, q4)
        else
          (q1, q2, succ q3, q4)
      else
        if y < mid_height then
          (q1, succ q2, q3, q4)
        else
          (q1, q2, q3, succ q4) in
  let q1, q2, q3, q4 =
    List.fold_left (move_and_count 100) (0, 0, 0, 0) robots in
  q1 * q2 * q3 * q4

let rec search ((width, height, robots) as input) n =
  let move ((x, y), (vx, vy)) =
    let wrap v m = let v = v mod m in if v < 0 then v + m else v in
    let x, y = wrap (x + n * vx) width, wrap (y + n * vy) height in
    y, x in
  let robots = List.sort compare @@ List.map move robots in
  let analyse (max, x', y', current) (y, x) =
    if y = y' && x = x' then
      (max, succ x, y, succ current)
    else if current > max then
      (current, succ x, y, 1)
    else
      (max, succ x, y, 1) in
  let biggest, _, _, current = List.fold_left analyse (0, 0, 0, 0) robots in
  if Int.max biggest current > 10 then
    n
  else
    search input (succ n)

let part2 input = search input 1

let () =
  Printf.printf "Day 14; Puzzle 1; test = %d\n\
                 Day 14; Puzzle 1 = %d\n\
                 Day 14; Puzzle 2 = %d\n" (part1 test) (part1 input)
                                          (part2 input)
