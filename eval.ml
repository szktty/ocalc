open Printf

exception Error of (int * string)

type t = {
  mutable stack : Value.t list
}

let create () = { stack = [] }

let push eval v =
  eval.stack <- v :: eval.stack

let top eval =
  match eval.stack with
  | [] -> None
  | hd :: _ -> Some hd

let pop eval =
  match eval.stack with
  | [] -> None
  | hd :: tl -> 
    eval.stack <- tl;
    Some hd

let clear eval =
  eval.stack <- []

let commands = [
  ("clear", clear);
  ("help", fun _ -> printf "help message\n"); (* TODO *)
  ("ls", fun eval ->
     ignore @@ List.fold_left
                 (fun i v ->
                    printf "%d: %s\n" i (Value.to_string v);
                    i - 1)
                 ((List.length eval.stack) - 1)
                 eval.stack);
  ("top", fun eval ->
     match top eval with
     | None -> printf "error: stack is empty\n"
     | Some v -> printf "%s\n" (Value.to_string v));
]

let run eval s =
  let f node =
    let open Ast in

    let error msg =
      raise (Error (node.loc_pos, msg))
    in

    let pop_exn () =
      match pop eval with
      | None -> raise (Error (node.loc_pos,"stack is empty"))
      | Some v -> v
    in

    let pop2_exn () =
      let y = pop_exn () in
      let x = pop_exn () in
      (x, y)
    in

    let arith_op f_int f_float =
      let v = match pop2_exn () with
        | (Value.Int x, Value.Int y) -> Value.Int (f_int x y)
        | (Value.Int iv, Value.Float fv)
        | (Value.Float fv, Value.Int iv) ->
          Value.Float (f_float fv @@ float_of_int iv)
        | (Value.Float x, Value.Float y) -> Value.Float (f_float x y)
        | _ -> error "invalid value"
      in
      push eval v
    in

    let apply name =
      if List.mem_assoc name commands then
        (List.assoc name commands) eval
      else
        push eval @@ Value.Atom name
    in

    match node.loc_desc with
    | Int v -> push eval (Value.Int v)
    | Float v -> push eval (Value.Float v)
    | Add -> arith_op (+) (+.)
    | Sub -> arith_op (-) (-.)
    | Atom name -> apply name
    | _ -> error "unknown operator"
  in

  let lexbuf = Lexing.from_string s in
  try begin
     let nodes = Parser.command Lexer.read lexbuf in
     List.iter f nodes
  end with
  | Parser.Error -> raise (Error (lexbuf.lex_start_pos, "invalid syntax"))
  | exn -> raise exn
