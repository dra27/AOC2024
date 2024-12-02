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

let rec is_safe min max last = function
| this::rest ->
    let change = this - last in
    change >= min && change <= max && is_safe min max this rest
| [] -> true

let is_safe = function
| a::((b::_) as rest) ->
    let min, max = if a > b then (-3, -1) else (1, 3) in
    is_safe min max a rest
| [] | [_] -> assert false

let part1 reports =
  let gather safe report =
    if is_safe report then
      succ safe
    else
      safe in
  List.fold_left gather 0 reports

let () =
  Printf.printf "Day 2; Puzzle 1; test = %d\n\
                 Day 2; Puzzle 1 = %d\n" (part1 test) (part1 input)
