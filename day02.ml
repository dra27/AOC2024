let get_input folder input =
  let gather acc line =
    (List.map int_of_string @@ String.split_on_char ' ' line) :: acc in
  folder gather [] input

let test =
  get_input List.fold_left @@ String.split_on_char '\n' (String.trim {|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|})

let input =
  In_channel.with_open_text "input-02" @@ fun ic ->
    get_input In_channel.fold_lines ic

let rec is_safe_without_dampener min max last = function
| this::rest ->
    let change = this - last in
    change >= min && change <= max && is_safe_without_dampener min max this rest
| [] -> true

let rec is_safe_with_dampener min max last = function
| this::rest ->
    let change = this - last in
    change >= min && change <= max && is_safe_with_dampener min max this rest
    || is_safe_without_dampener min max last rest
| [] -> true

let is_safe_without_dampener = function
| a::((b::_) as rest) ->
    let min, max = if a > b then (-3, -1) else (1, 3) in
    is_safe_without_dampener min max a rest
| [] | [_] -> assert false

let is_safe_with_dampener = function
| a::((b::((c::_) as rest2) as rest1)) ->
    let min, max = if a > b then (-3, -1) else (1, 3) in
    is_safe_with_dampener min max a rest1 ||
    is_safe_without_dampener (a::rest2) ||
    is_safe_without_dampener rest1
| [] | [_] | [_; _] -> assert false

let part is_safe reports =
  let gather safe report =
    if is_safe report then
      succ safe
    else
      safe in
  List.fold_left gather 0 reports

let part1 = part is_safe_without_dampener

let part2 = part is_safe_with_dampener

let () =
  Printf.printf "Day 2; Puzzle 1; test = %d\n\
                 Day 2; Puzzle 1 = %d\n\
                 Day 2; Puzzle 2; test = %d\n\
                 Day 2; Puzzle 2 = %d\n" (part1 test) (part1 input)
                                         (part2 test) (part2 input)
