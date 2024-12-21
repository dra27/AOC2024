module String = struct
  include String

  let foldi_right f a x =
    let r = ref x in
    for i = length a - 1 downto 0 do
      r := f i (unsafe_get a i) !r
    done;
    !r
end

let rec compute checksum id length index =
  if length = 0 then
    checksum
  else
    compute (checksum + id * index) id (pred length) (succ index)

let part1 input =
  let len = String.length input in
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

let compute_size input =
  String.fold_left (fun acc c -> acc + int_of_char c - 48) 0 input

let compute_space block_count input =
  let gather size_class i c (block, acc) =
    let size = int_of_char c - 48 in
    let block = block - size in
    if i land 1 = 1 && size = size_class then
      (block, block::acc)
    else
      (block, acc) in
  let f map size_class =
    let (_, freelist) =
      String.foldi_right (gather size_class) input (block_count, []) in
    if freelist = [] then
      map
    else
      (size_class, freelist)::map in
  List.fold_left f [] [9; 8; 7; 6; 5; 4; 3; 2; 1]

let part2 input =
  let block_count = compute_size input in
  let freelist = compute_space block_count input in
  let gather i c (block, freelist, checksum) =
    let size = int_of_char c - 48 in
    let block = block - size in
    if i land 1 = 0 then
      let find ((from, best) as acc) (size_class, entries) =
        if size_class >= size then
          let this = List.hd entries in
          if this < best then
            (size_class, this)
          else
            acc
        else
          acc in
      let (from, index) = List.fold_left find (0, block) freelist in
      let freelist =
        if from <> 0 then
          let rec update spare classes =
            match classes with
            | (size_class, entries)::rest when size_class = from ->
                let entries = List.tl entries in
                if entries = [] then
                  update spare rest
                else
                  (size_class, entries)::rest
            | (size_class, entries)::rest when size_class = spare ->
                let index = index + size in
                let[@tail_mod_cons] rec insert = function
                | [] ->
                    [index]
                | x :: rest when x < index ->
                    x :: insert rest
                | rest ->
                    index :: rest in
                (size_class, insert entries) :: update 0 rest
            | (size_class, entries)::rest
              when spare <> 0 && size_class > spare ->
                (spare, [index + size]) :: update 0 classes
            | this::rest ->
                this :: update spare rest
            | [] ->
                [] in
          update (from - size) freelist
        else
          freelist in
      block, freelist, compute checksum (i / 2) size index
    else
      block, freelist, checksum in
  let _, _, checksum =
    String.foldi_right gather input (block_count, freelist, 0) in
  checksum

let test = "2333133121414131402"
let input = In_channel.with_open_text "input-09" input_line

let () =
  Printf.printf "Day 9; Puzzle 1; test = %d\n\
                 Day 9; Puzzle 1 = %d\n\
                 Day 9; Puzzle 2; test = %d\n\
                 Day 9; Puzzle 2 = %d\n" (part1 test) (part1 input)
                                         (part2 test) (part2 input)
