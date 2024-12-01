let get_input folder input =
  let gather (left, right) line =
    Scanf.sscanf line "%u %u" (fun l r -> l::left, r::right) in
  let left, right =
    folder gather ([], []) input in
  List.sort Int.compare left, List.sort Int.compare right

let test =
  get_input List.fold_left @@ String.split_on_char '\n' (String.trim {|
3   4
4   3
2   5
1   3
3   9
3   3
|})

let input =
  In_channel.with_open_text "input-01" @@ fun ic ->
    get_input In_channel.fold_lines ic

let part1 (left, right) =
  let gather acc left right = acc + Int.abs (left - right) in
  List.fold_left2 gather 0 left right

let () =
  Printf.printf "Day 1; Puzzle 1; test = %d\n\
                 Day 1; Puzzle 1 = %d\n" (part1 test) (part1 input)
