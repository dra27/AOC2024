module Array = struct
  include Array

  let foldi_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r i (unsafe_get a i)
    done;
    !r
end

module String = struct
  include String

  let foldi_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r i (unsafe_get a i)
    done;
    !r
end

let test =
  Array.of_list @@ String.split_on_char '\n' (String.trim {|
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|})

let input =
 Array.of_list @@ In_channel.with_open_text "input-04" In_channel.input_lines

let part1 input =
  let y_max = Array.length input - 4 in
  let x_max = String.length input.(0) - 4 in
  let gather_row count y row =
    let gather_col count x _ =
      let test f ~cond count =
        if cond then
          let s = String.init 4 f in
          if s = "XMAS" || s = "SAMX" then
            succ count
          else
            count
        else
          count in
      count
      |> test (fun i -> input.(y).[x + i]) ~cond:(x <= x_max)
      |> test (fun i -> input.(y + i).[x]) ~cond:(y <= y_max)
      |> test (fun i -> input.(y + i).[x + i]) ~cond:(x <= x_max && y <= y_max)
      |> test (fun i -> input.(y + i).[x - i]) ~cond:(x >= 3 && y <= y_max) in
    String.foldi_left gather_col count row in
  Array.foldi_left gather_row 0 input

let () =
  Printf.printf "Day 4; Puzzle 1; test = %d\n\
                 Day 4; Puzzle 1 = %d\n" (part1 test) (part1 input)
