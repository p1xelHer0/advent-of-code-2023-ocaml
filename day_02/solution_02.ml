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

    let empty = make ~red:0 ~green:0 ~blue:0
    let incr_red i t = { t with red = t.red + i }
    let incr_green i t = { t with green = t.green + i }
    let incr_blue i t = { t with blue = t.blue + i }
  end

  type t = { id : int; rounds : Round.t list }
end

module P = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let int = take_while1 is_digit >>| Int.of_string_exn
  let newline = string "\n"
  let comma = string ", "
  let semicolon = string "; "
  let game_id = string "Game " *> int <* string ": "
  let red = int <* string " red" >>| fun n -> `Red n
  let green = int <* string " green" >>| fun n -> `Green n
  let blue = int <* string " blue" >>| fun n -> `Blue n
  let possible_colors = red <|> green <|> blue

  let round =
    sep_by comma possible_colors
    >>| Game.Round.(
          List.fold_left
            ~f:(fun x y ->
              match y with
              | `Red i -> incr_red i x
              | `Green i -> incr_green i x
              | `Blue i -> incr_blue i x)
            ~init:empty)

  let all_rounds = sep_by semicolon round

  let game =
    sep_by newline
      ( game_id >>= fun id ->
        all_rounds >>| fun rounds -> Game.{ id; rounds } )

  let parse input =
    parse_string ~consume:All game input |> Result.get_or_failwith
end

module Part_1 = struct
  let solve input =
    let open Game in
    let games = P.parse input in
    let possible game =
      List.for_all
        ~f:(fun Round.{ red; green; blue } ->
          red <= 12 && green <= 13 && blue <= 14)
        game.rounds
    in
    let possible_games = List.filter ~f:possible games in
    List.fold_left ~f:(fun i g -> i + g.id) ~init:0 possible_games

  let%test "sample data" = Test.(run int (solve sample) ~expect:8)
end

module Part_2 = struct
  let solve input =
    let open Game in
    let open Round in
    let games = P.parse input in
    let max_gems =
      List.fold_left
        ~f:(fun r1 r2 ->
          make ~red:(max r1.red r2.red) ~green:(max r1.green r2.green)
            ~blue:(max r1.blue r2.blue))
        ~init:empty
    in
    games
    |> List.map ~f:(fun g ->
           max_gems g.rounds |> fun g -> g.red * g.green * g.blue)
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:2286)
end

let run_1 () = Run.solve_int (module Part_1)
let run_2 () = Run.solve_int (module Part_2)
