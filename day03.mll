let digit = ['0'-'9']+

rule part1 acc = parse
| "mul(" (digit+ as a) ',' (digit+ as b) ')'
    {part1 (int_of_string a * int_of_string b + acc) lexbuf}

| _
    {part1 acc lexbuf}

| eof
    {acc}

{
let test =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let test_part1 = part1 0 @@ Lexing.from_string test
let solution_part1 =
  In_channel.with_open_text "input-03" @@ fun ic ->
    part1 0 (Lexing.from_channel ic)

let () =
  Printf.printf "Day 3; Puzzle 1; test = %d\n\
                 Day 3; Puzzle 1 = %d\n" test_part1 solution_part1
}
