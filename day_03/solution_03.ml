let sample =
  {|
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
|}
  |> String.trim

type number = { value : int; xmin : int; xmax : int; y : int }
[@@deriving show { with_path = false }]

type symbol = { char : char; x : int; y : int }
[@@deriving show { with_path = false }]

let adjacent n s =
  s.x >= pred n.xmin && s.x <= succ n.xmax && s.y >= pred n.y && s.y <= succ n.y

type p = { numbers : number list; symbols : symbol list }
[@@deriving show { with_path = false }]

let parse input =
  let rec parse_aux line number ~numbers ~symbols ~x ~y =
    match line with
    | ('0' .. '9' as c1) :: ('0' .. '9' as c2) :: tail ->
        parse_aux (c2 :: tail) (number @ [ c1 ]) ~numbers ~symbols ~x:(succ x)
          ~y
    | ('0' .. '9' as c) :: tail ->
        let value = number @ [ c ] |> String.of_list |> Int.of_string_exn in
        let xmin = x - List.length number in
        parse_aux tail []
          ~numbers:(numbers @ [ { value; xmin; xmax = x; y } ])
          ~symbols ~x:(succ x) ~y
    | '.' :: tail -> parse_aux tail number ~numbers ~symbols ~x:(succ x) ~y
    | symbol :: tail ->
        let symbol = { char = symbol; x; y } in
        parse_aux tail number ~numbers ~symbols:(symbols @ [ symbol ])
          ~x:(succ x) ~y
    | [] -> { numbers; symbols }
  in
  let char_grid = input |> String.lines |> List.map ~f:String.to_list in
  char_grid
  |> List.mapi ~f:(fun y line ->
         parse_aux line [] ~numbers:[] ~symbols:[] ~x:0 ~y)
  |> List.fold_left
       ~f:(fun s1 s2 ->
         {
           numbers = s1.numbers @ s2.numbers;
           symbols = s1.symbols @ s2.symbols;
         })
       ~init:{ numbers = []; symbols = [] }

module Part_1 = struct
  let solve input =
    let parsed = parse input in
    parsed.numbers
    |> List.filter ~f:(fun number ->
           List.exists ~f:(fun symbol -> adjacent number symbol) parsed.symbols)
    |> List.map ~f:(fun number -> number.value)
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:4361)
end

module Part_2 = struct
  let solve input =
    let parsed = parse input in
    parsed.symbols
    |> List.filter_map ~f:(fun symbol ->
           let adjacents =
             List.filter
               ~f:(fun number -> adjacent number symbol)
               parsed.numbers
           in
           match (symbol.char, adjacents) with
           | '*', [ { value = v1; _ }; { value = v2; _ } ] -> Some (v1 * v2)
           | _ -> None)
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:467835)
end

let run_1 () = Run.solve_int (module Part_1)
let run_2 () = Run.solve_int (module Part_2)
