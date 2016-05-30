type 'a loc = {
  loc_pos : int;
  loc_desc : 'a;
}

type t = desc loc

and desc =
  | Int of int
  | Float of float
  | Symbol of string
  | Command of string
  | Add
  | Add_all
  | Sub
  | Sub_all
  | Mul
  | Mul_all
  | Div
  | Pow
  | Rem
  | Bang

let with_loc loc desc =
  { loc_pos = loc; loc_desc = desc }

let of_loc loc desc =
  with_loc loc.loc_pos desc
