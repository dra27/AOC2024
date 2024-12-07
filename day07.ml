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

let possible op (total, calibrations) =
  let rec loop current calibrations =
    let test op x xs =
      let next = op current x in
      next <= total && loop next xs in
    match calibrations with
    | [] -> current = total
    | x::xs ->
        test ( * ) x xs || test (+) x xs || test op x xs in
  match calibrations with
  | x::xs -> loop x xs
  | [] -> assert false

let gather op input =
  let gather acc ((total, _) as calibration) =
    if possible op calibration then
      acc + total
    else
      acc in
  List.fold_left gather 0 input

let concat a b =
  let mul =
    int_of_float (Float.pow 10. (floor (log10 (float_of_int b)) +. 1.)) in
  a * mul + b

let part1 = gather (fun _ _ -> max_int)
let part2 = gather concat

let () =
  Printf.printf "Day 7; Puzzle 1; test = %d\n\
                 Day 7; Puzzle 1 = %d\n\
                 Day 7; Puzzle 2; test = %d\n\
                 Day 7; Puzzle 2 = %d\n" (part1 test) (part1 input)
                                         (part2 test) (part2 input)
