module PairMap = Map.Make(struct type t = int * int let compare = compare end)

let rec blink cache x n =
  try cache, PairMap.find (x, n) cache
  with Not_found ->
    let s_x = string_of_int x in
    let l = String.length s_x in
    if l land 1 = 0 then
      if n = 1 then
        cache, 2
      else
        let cache, count1 =
          let x_upper = int_of_string (String.sub s_x 0 (l lsr 1)) in
          blink cache x_upper (pred n) in
        let cache, count2 =
          let x_lower = int_of_string (String.sub s_x (l lsr 1) (l lsr 1)) in
          blink cache x_lower (pred n) in
        let count = count1 + count2 in
        PairMap.add (x, n) count cache, count
    else if n = 1 then
      cache, 1
    else
      blink cache (if x = 0 then 1 else 2024 * x) (pred n)

let part n input =
  let f (cache, result) x =
    let cache, this = blink cache x n in
    cache, this + result in
  snd @@ List.fold_left f (PairMap.empty, 0) input

let part1 = part 25
let part2 = part 75

let test =
  List.map int_of_string @@ String.split_on_char ' ' "125 17"

let input =
  In_channel.with_open_text "input-11" @@ fun ic ->
    List.map int_of_string @@ String.split_on_char ' ' (input_line ic)

let () =
  Printf.printf "Day 11; Puzzle 1; test = %d\n\
                 Day 11; Puzzle 1 = %d\n\
                 Day 11; Puzzle 2; test = %d\n\
                 Day 11; Puzzle 2 = %d\n" (part1 test) (part1 input)
                                          (part2 test) (part2 input)
