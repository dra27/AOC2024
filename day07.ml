let parse folder input =
  let gather acc line =
    let calibration =
      Scanf.sscanf line "%u: %s@!%!" @@ fun a b ->
        a, List.map int_of_string @@ String.split_on_char ' ' b in
    calibration::acc in
  folder gather [] input

let test =
  parse List.fold_left @@ String.split_on_char '\n' (String.trim {|
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|})

let input =
  In_channel.with_open_text "input-07" @@ fun ic ->
    parse In_channel.fold_lines ic

let possible (total, calibrations) =
  let rec loop current = function
  | [] -> current = total
  | x::xs ->
      (current * x <= total && loop (current * x) xs) ||
      (current + x <= total && loop (current + x) xs) in
  match calibrations with
  | x::xs -> loop x xs
  | [] -> assert false

let part1 input =
  let gather acc ((total, _) as calibration) =
    if possible calibration then
      acc + total
    else
      acc in
  List.fold_left gather 0 input

let () =
  Printf.printf "Day 7; Puzzle 1; test = %d\n\
                 Day 7; Puzzle 1 = %d\n" (part1 test) (part1 input)
