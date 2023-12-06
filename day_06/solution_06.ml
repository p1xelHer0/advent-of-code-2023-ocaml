let sample = {|
Time:      7  15   30
Distance:  9  40  200
|} |> String.trim

type race = { time : int; distance : int }
[@@deriving show { with_path = false }, eq]

module P = struct
  open Angstrom

  (* DO NOT LOOK PLEASE *)
  let is_digit = function '0' .. '9' -> true | _ -> false
  let digits = take_while1 is_digit
  let is_whitespace = function ' ' -> true | _ -> false
  let whitespace = take_while is_whitespace
  let numbers = sep_by whitespace digits
  let num = list
  let time = string "Time:" *> whitespace *> numbers <* string "\n"
  let distance = string "Distance:" *> whitespace *> numbers

  let races kerning =
    let* time = time in
    let+ distance = distance in
    match (time, distance) with
    (* DO NOT LOOK PLEASE *)
    | t, d when List.(length t = length d) ->
        let t' =
          if kerning then
            List.fold_left ~f:( ^ ) ~init:"" t
            |> List.pure |> List.map ~f:int_of_string
          else List.map ~f:int_of_string t
        in
        let d' =
          if kerning then
            d
            |> List.fold_left ~f:( ^ ) ~init:""
            |> List.pure |> List.map ~f:int_of_string
          else List.map ~f:int_of_string d
        in
        List.map2 ~f:(fun time distance -> { time; distance }) t' d'
    | _ -> failwith "u succ @ parsin'"

  let parse input ~kerning =
    parse_string ~consume:All (races kerning) input |> Result.get_or_failwith
end

let ways_to_win { time; distance } =
  let try_race button_held =
    let travel_time = time - button_held in
    let travel_distance = travel_time * button_held in
    if travel_distance > distance then Some button_held else None
  in
  List.(1 --^ time |> filter_map ~f:try_race)

let solve_aux races =
  List.(
    races |> map ~f:ways_to_win |> map ~f:List.length
    |> fold_left ~f:( * ) ~init:1)

module Part_1 = struct
  let solve input =
    let races = P.parse input ~kerning:false in
    solve_aux races

  let%test "sample data" = Test.(run int (solve sample) ~expect:288)
end

module Part_2 = struct
  let solve input =
    let races = P.parse input ~kerning:true in
    solve_aux races

  let%test "sample data" = Test.(run int (solve sample) ~expect:71503)
end

let run_1 () = Run.solve_int (module Part_1)
let run_2 () = Run.solve_int (module Part_2)
