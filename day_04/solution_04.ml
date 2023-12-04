let sample =
  {|
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|}
  |> String.trim

module P = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let int = take_while1 is_digit >>| Int.of_string_exn

  let is_whitespace = function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false

  let whitespace = take_while is_whitespace
  let score = whitespace *> int <* whitespace
  let x = sep_by whitespace
  let round = sep_by (string " | ")
  let card = string "Card " *> int <* string ": "
end

module Part_1 = struct
  let solve input = 0
  let%test "sample data" = Test.(run int (solve sample) ~expect:13)
end

module Part_2 = struct
  let solve input = 0
  (* let%test "sample data" = Test.(run int (solve sample) ~expect:0) *)
end

let run_1 () =
  (* Run.solve_int (module Part_1); *)
  ()

let run_2 () =
  (* Run.solve_int (module Part_2); *)
  ()
