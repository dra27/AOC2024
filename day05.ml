module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

let rec parse_rules rules = function
| ""::rest ->
  rules, rest
| rule::rest ->
    Scanf.sscanf rule "%u|%u" @@ fun l r ->
      let set =
        match IntMap.find r rules with
        | set -> set
        | exception Not_found -> IntSet.empty in
      parse_rules (IntMap.add r (IntSet.add l set) rules) rest
| [] -> assert false

let test =
  parse_rules IntMap.empty @@ String.split_on_char '\n' (String.trim {|
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|})

let input =
  In_channel.with_open_text "input-05" @@ fun ic ->
    parse_rules IntMap.empty @@ In_channel.input_lines ic

let process_updates gather initial (rules, updates) =
  let process acc l =
    gather rules acc @@ List.map int_of_string (String.split_on_char ',' l) in
  List.fold_left process initial updates

let part1 rules (count, incorrect) update =
  let middle = List.length update / 2 in
  let is_valid (not_allowed_now, middle_element) elt =
    if IntSet.mem elt not_allowed_now then
      raise Exit
    else
      let not_allowed_now =
        IntMap.find_opt elt rules
        |> Option.map (IntSet.union not_allowed_now)
        |> Option.value ~default:not_allowed_now in
      let middle_element =
        if middle_element < 0 then
          succ middle_element
        else if middle_element = 0 then
          elt
        else
          middle_element in
      (not_allowed_now, middle_element) in
  try
    let count =
      count + snd (List.fold_left is_valid (IntSet.empty, -middle) update) in
    count, incorrect
  with Exit -> count, update::incorrect

let part1 ((rules, _) as input) =
  let (count, incorrect) = process_updates part1 (0, []) input in
  count, (rules, incorrect)

let (test_part1, test_incorrect) = part1 test
let (solution_part1, input_incorrect) = part1 input

let part2 rules count update =
  let[@tail_mod_cons] rec correct = function
  | [] -> []
  | elt::rest ->
      let[@tail_mod_cons] process set =
        let hoist, rest = List.partition (Fun.flip IntSet.mem set) rest in
        if hoist = [] then
          elt :: correct rest
        else
          correct (hoist @ elt::rest) in
      match IntMap.find_opt elt rules with
      | Some set -> process set
      | None -> elt :: correct rest in
  let corrected = correct update in
  count + List.nth corrected (List.length corrected / 2)

let part2 (rules, updates) = List.fold_left (part2 rules) 0 updates

let test_part2 = part2 test_incorrect
let solution_part2 = part2 input_incorrect

let () =
  Printf.printf "Day 5; Puzzle 1; test = %d\n\
                 Day 5; Puzzle 1 = %d\n\
                 Day 5; Puzzle 2; test = %d\n\
                 Day 5; Puzzle 2 = %d\n" test_part1 solution_part1
                                         test_part2 solution_part2
