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

module String = struct
  include String

  let foldi_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r i (unsafe_get a i)
    done;
    !r
end

let parse input =
  let len = List.length input in
  let f lines i =
    if i = 0 || i = len + 1 then
      lines, String.make (len + 2) ' '
    else
      match lines with
      | line::lines ->
          let f i =
            if i = 0 || i = len + 1 then
              ' '
            else
              line.[i - 1] in
          lines, String.init (len + 2) f
      | [] ->
          assert false in
  snd @@ Array.init_fold (len + 2) f input

let part1 input =
  let last = Array.length input - 1 in
  let seen =
    let f y =
      let mark = (y = 0 || y = last) in
      Array.init (Array.length input) (fun x -> mark || x = 0 || x = last) in
    Array.init (Array.length input) f in
  let rec measure c x y ((perimeter, area) as acc) =
    if input.(y).[x] = c then
      if seen.(y).(x) then
        acc
      else begin
        seen.(y).(x) <- true;
        (perimeter, succ area)
        |> measure c x (pred y)
        |> measure c x (succ y)
        |> measure c (pred x) y
        |> measure c (succ x) y
      end
    else
      (succ perimeter, area) in
  let gather cost y row =
    let gather cost x c =
      if seen.(y).(x) then
        cost
      else
        let perimeter, area = measure c x y (0, 0) in
        cost + perimeter * area in
    String.foldi_left gather cost row in
  Array.foldi_left gather 0 input

let test =
  parse @@ String.split_on_char '\n' (String.trim {|
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
|})

let input =
  In_channel.with_open_text "input-12" @@ fun ic ->
    parse @@ In_channel.input_lines ic

let () =
  Printf.printf "Day 12; Puzzle 1; test = %d\n\
                 Day 12; Puzzle 1 = %d\n" (part1 test) (part1 input)
