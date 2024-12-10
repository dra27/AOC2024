module String = struct
  include String

  let foldi_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r i (unsafe_get a i)
    done;
    !r
end

module CharMap = Map.Make(Char)

let gather folder input =
  let gather (freqs, y) line =
    let gather freqs x c =
      if c = '.' then
        freqs
      else
        let antennae =
          try CharMap.find c freqs
          with Not_found -> [] in
        CharMap.add c ((x, y)::antennae) freqs in
    let freqs = String.foldi_left gather freqs line in
    (freqs, succ y) in
  folder gather (CharMap.empty, 0) input

let test =
  gather List.fold_left @@ String.split_on_char '\n' (String.trim {|
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|})

let input = In_channel.with_open_text "input-08" @@ gather In_channel.fold_lines

module PairSet = Set.Make(struct type t = int * int let compare = compare end)

let part1 (freqs, max) =
  let add x y antennae =
    if x < 0 || y < 0 || x >= max || y >= max then
      antennae
    else
      PairSet.add (x, y) antennae in
  let rec augment antinodes x y = function
  | (x', y')::antennae ->
      let antinodes =
        assert (y > y');
        let dy = y' - y in
        let dx = x' - x in
        add (x - dx) (y - dy) (add (x' + dx) (y' + dy) antinodes) in
      augment (augment antinodes x' y' antennae) x y antennae
  | [] ->
      antinodes in
  let fold _ antennae antinodes =
    match antennae with
    | (x, y)::antennae ->
        augment antinodes x y antennae
    | [] ->
        assert false in
  PairSet.cardinal @@ CharMap.fold fold freqs PairSet.empty

let () =
  Printf.printf "Day 8; Puzzle 1; test = %d\n\
                 Day 8; Puzzle 1 = %d\n" (part1 test) (part1 input)
