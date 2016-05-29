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

    let _operate node =
      let open Ast in
      match node.loc_desc with
      | Ast.Add ->
        begin match pop2_exn () with
        | (Value.Int x, Value.Int y) -> push eval (Value.Int (x+y))
        | _ -> failwith "not impl"
        end
      | _ -> raise (Error (node.loc_pos, "error: unknown operator"))
    in
      ()
  in

  let lexbuf = Lexing.from_string s in
  try begin
     let nodes = Parser.command Lexer.read lexbuf in
     List.iter f nodes
  end with
  | Parser.Error -> raise (Error (lexbuf.lex_start_pos, "invalid syntax"))
  | exn -> raise exn
