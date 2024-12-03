let is_digit = function '0'..'9' -> true | _ -> false

type state =
  Skip | M | U | L | LParen | A of int | Comma of int | B of int * int

let part1 input =
  let gather ((state, total) as acc) c =
    match state with
    | Skip when c = 'm' -> (M, total)
    | M when c = 'u' -> (U, total)
    | U when c = 'l' -> (L, total)
    | L when c = '(' -> (LParen, total)
    | LParen when is_digit c ->
        (A(int_of_char c - 48), total)
    | A current when is_digit c ->
        (A(current * 10 + int_of_char c - 48), total)
    | A a when c = ',' -> (Comma a, total)
    | Comma a when is_digit c ->
        (B(a, int_of_char c - 48), total)
    | B(a, current) when is_digit c ->
        (B(a, current * 10 + int_of_char c - 48), total)
    | B(a, b) when c = ')' -> (Skip, total + a * b)
    | Skip -> acc
    | _ -> (Skip, total) in
  snd @@ String.fold_left gather (Skip, 0) input

let test =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let input = In_channel.with_open_text "input-03" In_channel.input_all

let () =
  Printf.printf "Day 3; Puzzle 1; test = %d\n\
                 Day 3; Puzzle 1 = %d\n" (part1 test) (part1 input)
