let rec blink = function
| 0 :: stones ->
    1 :: blink stones
| x :: stones when String.length (string_of_int x) land 1 = 0 ->
    let x = string_of_int x in
    let l = String.length x in
    int_of_string (String.sub x 0 (l lsr 1)) ::
    int_of_string (String.sub x (l lsr 1) (l lsr 1)) ::
    blink stones
| x :: stones ->
    2024 * x :: blink stones
| [] ->
    []

let part1 =
  let rec loop n l =
    if n = 0 then
      List.length l
    else
      loop (pred n) (blink l) in
  loop 25

let test =
  List.map int_of_string @@ String.split_on_char ' ' "125 17"

let input =
  In_channel.with_open_text "input-11" @@ fun ic ->
    List.map int_of_string @@ String.split_on_char ' ' (input_line ic)

let () =
  Printf.printf "Day 11; Puzzle 1; test = %d\n\
                 Day 11; Puzzle 1 = %d\n" (part1 test) (part1 input)
