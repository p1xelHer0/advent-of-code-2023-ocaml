module type Solution_Int = sig
  val solve : string -> int
end

module type Solution_String = sig
  val solve : string -> string
end

val solve_int : (module Solution_Int) -> unit
val solve_string : (module Solution_String) -> unit
