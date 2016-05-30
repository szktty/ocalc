type t =
  | Int of int
  | Float of float
  | Symbol of string

let to_string = function
  | Int v -> string_of_int v
  | Float v -> string_of_float v
  | Symbol name -> "#" ^ name
