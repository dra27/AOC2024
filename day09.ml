let part1 input =
  let len = String.length input in
  let rec compute checksum id length index =
    if length = 0 then
      checksum
    else
      compute (checksum + id * index) id (pred length) (succ index) in
  let rec gather checksum l r l_id r_id r_len index =
    if l > r then
      compute checksum r_id r_len index
    else
      let len = int_of_char input.[l] - 48 in
      assert (len <> 0);
      let checksum = compute checksum l_id len index in
      let index = index + len in
      let l = succ l in
      let len = int_of_char input.[l] - 48 in
      defrag checksum l r (succ l_id) r_id r_len len index
  and defrag checksum l r l_id r_id r_len length index =
    if length = 0 then
      gather checksum (succ l) r l_id r_id r_len index
    else if r_len = 0 then
      let r_len = int_of_char input.[r] - 48 in
      defrag checksum l (r - 2) l_id (pred r_id) r_len length index
    else
      let checksum = checksum + r_id * index in
      defrag checksum l r l_id r_id (pred r_len) (pred length) (succ index)
  in
  gather 0 0 (pred len) 0 ((succ len) lsr 1) 0 0

let test = "2333133121414131402"
let input = In_channel.with_open_text "input-09" input_line

let () =
  Printf.printf "Day 9; Puzzle 1; test = %d\n\
                 Day 9; Puzzle 1 = %d\n" (part1 test) (part1 input)
