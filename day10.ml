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

let parse input =
  let len = List.length input in
  let f lines i =
    if i = 0 || i = len + 1 then
      lines, Array.make (len + 2) (-1)
    else
      match lines with
      | line::lines ->
          let f i =
            if i = 0 || i = len + 1 then
              -1
            else
              int_of_char line.[i - 1] - 48 in
          lines, Array.init (len + 2) f
      | [] ->
          assert false in
  snd @@ Array.init_fold (len + 2) f input

module PairSet = Set.Make(struct type t = int * int let compare = compare end)

let part1 input =
  let rec walk v x y nines =
    if input.(y).(x) = v then
      if v = 9 then
        PairSet.add (x, y) nines
      else
        let v = succ v in
        nines
        |> walk v x (pred y)
        |> walk v x (succ y)
        |> walk v (pred x) y
        |> walk v (succ x) y
    else
      nines in
  let gather score y row =
    let gather score x v =
      if v = 0 then
        score + PairSet.cardinal (walk 0 x y PairSet.empty)
      else
        score in
    Array.foldi_left gather score row in
  Array.foldi_left gather 0 input

let test =
  parse @@ String.split_on_char '\n' (String.trim {|
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|})

let input =
  In_channel.with_open_text "input-10" @@ fun ic ->
    parse @@ In_channel.input_lines ic

let () =
  Printf.printf "Day 10; Puzzle 1; test = %d\n\
                 Day 10; Puzzle 1 = %d\n" (part1 test) (part1 input)
