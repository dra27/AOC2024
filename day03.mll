{
type mode = Deactivated | Disabled | Enabled
}

let digit = ['0'-'9']+

rule part mode acc = parse
| "mul(" (digit+ as a) ',' (digit+ as b) ')'
    {let acc =
      if mode <> Disabled then
        int_of_string a * int_of_string b + acc
      else
        acc in
     part mode acc lexbuf}

| "do" ("n't" as dont)? "()"
    {let mode =
       if mode = Deactivated then
         Deactivated
       else if dont = None then
         Enabled
       else
         Disabled in
     part mode acc lexbuf}

| _
    {part mode acc lexbuf}

| eof
    {acc}

{
let test_part1 =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let test_part2 =
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let solution mode =
  In_channel.with_open_text "input-03" @@ fun ic ->
    part mode 0 (Lexing.from_channel ic)

let test_part1 = part Deactivated 0 @@ Lexing.from_string test_part1
let solution_part1 = solution Deactivated
let test_part2 = part Enabled 0 @@ Lexing.from_string test_part2
let solution_part2 = solution Enabled

let () =
  Printf.printf "Day 3; Puzzle 1; test = %d\n\
                 Day 3; Puzzle 1 = %d\n\
                 Day 3; Puzzle 2; test = %d\n\
                 Day 3; Puzzle 2 = %d" test_part1 solution_part1
                                       test_part2 solution_part2
}
