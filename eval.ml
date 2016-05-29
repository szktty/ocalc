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

let run eval s =
  let f node =
    let open Ast in

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

    let error msg =
      raise (Error (node.loc_pos, msg))
    in

    match node.loc_desc with
    | Int v -> push eval (Value.Int v)
    | Float v -> push eval (Value.Float v)
    | Add ->
      begin match pop2_exn () with
      | (Value.Int x, Value.Int y) -> push eval (Value.Int (x+y))
      | (Value.Int iv, Value.Float fv)
      | (Value.Float fv, Value.Int iv) ->
        let v = fv +. (float_of_int iv) in
        push eval (Value.Float v)
      | (Value.Float x, Value.Float y) -> push eval (Value.Float (x+.y))
      end
    | _ -> error "error: unknown operator"
  in

  let lexbuf = Lexing.from_string s in
  try begin
     let nodes = Parser.command Lexer.read lexbuf in
     List.iter f nodes
  end with
  | Parser.Error -> raise (Error (lexbuf.lex_start_pos, "invalid syntax"))
  | exn -> raise exn
