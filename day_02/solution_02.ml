let sample =
  {|
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|}
  |> String.trim

module Game = struct
  module Round = struct
    type t = { red : int; green : int; blue : int }
    [@@deriving show { with_path = false }, make, eq]

    let set_red x t = { t with red = x }
    let set_green x t = { t with green = x }
    let set_blue x t = { t with blue = x }
    let test = Test.testable pp equal

    let smoosh =
      List.fold_left
        ~f:(fun s1 s2 ->
          let red = s1.red + s2.red in
          let green = s1.green + s2.green in
          let blue = s1.blue + s2.blue in
          make ~red ~green ~blue)
        ~init:(make ~red:0 ~green:0 ~blue:0)

    let%test "set_color" =
      let set = make ~red:1 ~green:2 ~blue:3 in

      Test.(
        run test (set |> set_red 10) ~expect:(make ~red:10 ~green:2 ~blue:3));
      Test.(
        run test (set |> set_green 10) ~expect:(make ~red:1 ~green:10 ~blue:3));
      Test.(
        run test (set |> set_blue 10) ~expect:(make ~red:1 ~green:2 ~blue:10))
  end

  type t = { id : int; rounds : Round.t list }
  [@@deriving show { with_path = false }, eq, make]

  let test = Test.testable pp equal

  let count_red t =
    List.fold_left ~f:Round.(fun a t -> a + t.red) ~init:0 t.rounds

  let count_green t =
    List.fold_left ~f:Round.(fun a t -> a + t.green) ~init:0 t.rounds

  let count_blue t =
    List.fold_left ~f:Round.(fun a t -> a + t.blue) ~init:0 t.rounds

  let%test "count_color" =
    let game : t =
      make ~id:1
        ~rounds:
          [
            Set.make ~red:1 ~green:1 ~blue:1;
            Set.make ~red:2 ~green:1 ~blue:1;
            Set.make ~red:3 ~green:1 ~blue:1;
            Set.make ~red:4 ~green:0 ~blue:1;
            Set.make ~red:3 ~green:0 ~blue:3;
          ]
        ()
    in

    Test.(run Test.int (game |> count_red) ~expect:13);
    Test.(run Test.int (game |> count_green) ~expect:3);
    Test.(run Test.int (game |> count_blue) ~expect:7)
end

module P = struct
  open Angstrom

  let is_whitespace = function ' ' | '\t' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_char = function 'a' .. 'z' -> true | _ -> false
  let whitespace = take_while is_whitespace
  let number = take_while1 is_digit >>| Int.of_string_exn
  let game_id = string "Game " *> number <* string ": "

  let%test "parse game_id" =
    Test.(
      run
        (Test.result Test.int Test.string)
        (parse_string ~consume:All game_id "Game 13: ")
        ~expect:(Ok 13))

  let red =
    number <* string " red" >>| fun n ->
    Game.Set.{ red = n; green = 0; blue = 0 }

  let%test "parse red" =
    Test.(
      run
        (Test.result Game.Set.test Test.string)
        (parse_string ~consume:All red "4 red")
        ~expect:(Ok (Game.Set.make ~red:4 ~green:0 ~blue:0)))

  let green =
    number <* string " green" >>| fun n ->
    Game.Set.{ red = 0; green = n; blue = 0 }

  let%test "parse green" =
    Test.(
      run
        (Test.result Game.Set.test Test.string)
        (parse_string ~consume:All green "5 green")
        ~expect:(Ok (Game.Set.make ~red:0 ~green:5 ~blue:0)))

  let blue =
    number <* string " blue" >>| fun n ->
    Game.Set.{ red = 0; green = 0; blue = n }

  let%test "parse blue" =
    Test.(
      run
        (Test.result Game.Set.test Test.string)
        (parse_string ~consume:All blue "3 blue")
        ~expect:(Ok (Game.Set.make ~red:0 ~green:0 ~blue:3)))

  let cube = red <|> green <|> blue
  let sep = string ", "
  let end_ = string "; "
  let game_set_smooshed = sep_by sep cube >>| Game.Set.smoosh

  let%test "parse game_set_smooshed" =
    let text = "5 red, 10 blue, 20 green, 5 red, 10 blue, 10 blue" in
    Test.(
      run
        (Test.result Game.Set.test Test.string)
        (parse_string ~consume:All game_set_smooshed text)
        ~expect:(Ok (Game.Set.make ~red:10 ~green:20 ~blue:30)))

  let game_smooshed = sep_by end_ game_set_smooshed

  let%test "parse game" =
    let text =
      "5 red, 10 blue, 20 green, 5 red, 10 blue, 10 blue; 1 red, 1 green, 1 blue, 1 red; 1 blue"
    in
    Test.(
      run
        (Test.result (Test.list Game.Set.test) Test.string)
        (parse_string ~consume:All game_smooshed text)
        ~expect:
          (Ok
             [
               Game.Set.make ~red:10 ~green:20 ~blue:30;
               Game.Set.make ~red:2 ~green:1 ~blue:1;
               Game.Set.make ~red:0 ~green:0 ~blue:1;
             ]))

  let parse_a input =
    parse_string ~consume:All game_smooshed input |> Result.get_or_failwith

  let parsers =
    Util.
      [
        parse "Game %i: %s" (fun game_id input ->
            Game.{ id = game_id; rounds = parse_a input });
      ]

  let parse_instructions = Util.try_parse parsers
end

module Part_1 = struct
  let solve input =
    let lines = String.lines input in
    let parsed = List.map ~f:P.parse_instructions lines in
    List.iter ~f:(fun g -> print_endline (Game.show g)) parsed;
    0

  let%test "sample data" = Test.(run int (solve sample) ~expect:8)
end

module Part_2 = struct
  let solve input = 0
  let%test "sample data" = Test.(run int (solve sample) ~expect:0)
end

let run_1 () =
  (* Run.solve_int (module Part_1); *)
  ()

let run_2 () =
  (* Run.solve_int (module Part_2); *)
  ()
