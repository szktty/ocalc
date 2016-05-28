exception Error of string

type t = {
  mutable stack : Value.t list
}

let create () = { stack = [] }

let push env v =
  env.stack <- v :: env.stack

let top env =
  match env.stack with
  | [] -> None
  | hd :: _ -> Some hd

let pop env =
  match env.stack with
  | [] -> None
  | hd :: tl -> 
    env.stack <- tl;
    Some hd

let pop_exn env =
  match pop env with
  | None -> raise (Error "error: stack is empty")
  | Some v -> v

let pop2_exn env =
  let y = pop_exn env in
  let x = pop_exn env in
  (x, y)

let operate env node =
  match Ast.(node.loc_desc) with
  | Ast.Add ->
    begin match pop2_exn env with
    | (Value.Int x, Value.Int y) -> push env (Value.Int (x+y))
    | _ -> failwith "not impl"
    end
  | _ -> raise (Error "error: unknown operator")
