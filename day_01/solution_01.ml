let sum_first_last lines =
  let first = List.hd lines |> Char.to_string in
  let last = List.rev lines |> List.hd |> Char.to_string in
  Int.of_string_exn (first ^ last)

module Part_1 = struct
  let sample = {|
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|} |> String.trim

  let solve input =
    let lines = String.lines input in
    let is_digit = function '0' .. '9' -> true | _ -> false in

    let nums =
      lines |> List.map ~f:String.to_list
      |> List.map ~f:(List.filter ~f:is_digit)
    in

    nums |> List.map ~f:sum_first_last |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:142)
end

module Part_2 = struct
  let sample =
    {| 
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
|}
    |> String.trim

  let solve input =
    let chars = input |> String.lines |> List.map ~f:String.to_list in

    let rec solve_aux result line =
      match line with
      | 'o' :: 'n' :: 'e' :: _ -> solve_aux (result @ [ '1' ]) (List.tl line)
      | 't' :: 'w' :: 'o' :: _ -> solve_aux (result @ [ '2' ]) (List.tl line)
      | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ ->
          solve_aux (result @ [ '3' ]) (List.tl line)
      | 'f' :: 'o' :: 'u' :: 'r' :: _ ->
          solve_aux (result @ [ '4' ]) (List.tl line)
      | 'f' :: 'i' :: 'v' :: 'e' :: _ ->
          solve_aux (result @ [ '5' ]) (List.tl line)
      | 's' :: 'i' :: 'x' :: _ -> solve_aux (result @ [ '6' ]) (List.tl line)
      | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ ->
          solve_aux (result @ [ '7' ]) (List.tl line)
      | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ ->
          solve_aux (result @ [ '8' ]) (List.tl line)
      | 'n' :: 'i' :: 'n' :: 'e' :: _ ->
          solve_aux (result @ [ '9' ]) (List.tl line)
      | ('1' .. '9' as c) :: tail -> solve_aux (result @ [ c ]) tail
      | c :: tail -> solve_aux result tail
      | [] -> result
    in

    chars
    |> List.map ~f:(solve_aux [])
    |> List.map ~f:sum_first_last |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:281)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
