type t =
  | Int of int
  | Float of float

let to_string = function
  | Int v -> string_of_int v
  | Float v -> string_of_float v
