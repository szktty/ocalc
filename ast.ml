type 'a loc = {
  loc_offset : int;
  loc_desc : 'a;
}

type t = desc loc

and desc =
  | Int of int
  | Float of float
  | String of string
  | Command of string
  | Add
  | Sub
  | Mul
  | Div
  | Pow
  | Rem

let with_loc loc desc =
  { loc_offset = loc; loc_desc = desc }

let of_loc loc desc =
  with_loc loc.loc_offset desc
