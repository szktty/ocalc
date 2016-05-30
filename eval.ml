open Spotlib.Spot
open Printf

exception Error of (int option * string)

type t = {
  mutable stack : Value.t list
}

let error ?pos msg =
  raise (Error (pos, msg))

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

let pop_exn eval =
  match pop eval with
  | None -> error "the stack is empty"
  | Some v -> v

let pop2_exn eval =
  if List.length eval.stack > 1 then begin
    let y = pop_exn eval in
    let x = pop_exn eval in
    (x, y)
  end else
    error "the stack does not have 2 values"

let clear eval =
  eval.stack <- []

let arith_op eval f_int f_float =
  let v = match pop2_exn eval with
    | (Value.Int x, Value.Int y) -> Value.Int (f_int x y)
    | (Value.Int iv, Value.Float fv)
    | (Value.Float fv, Value.Int iv) ->
      Value.Float (f_float fv @@ float_of_int iv)
    | (Value.Float x, Value.Float y) -> Value.Float (f_float x y)
    | _ -> error "invalid value"
  in
  push eval v

let arith_all_op eval f_int f_float =
  while List.length eval.stack > 1 do
    arith_op eval f_int f_float
  done

let rec commands = [
  ("abs", "absolute value", fun eval ->
     let v = match pop_exn eval with
       | Value.Int v -> Value.Int (abs v)
       | Value.Float v -> Value.Float (abs_float v)
       | _ -> error "invalid value"
     in
     push eval v);
  ("c", "clear the stack", clear);
  ("exit", "exit this program", fun _ -> exit 0);
  ("h", "show this message", fun _ ->
     printf "commands:\n";
     List.iter (fun (name, doc, _) ->
                  printf "    %- 8s%s\n" name doc) commands;
     printf "\noperators:\n";
     printf "    +       addition\n";
     printf "    +!      addition (all values)\n";
     printf "    -       subtraction\n";
     printf "    -!      subtraction (all values)\n";
     printf "    *       multiplication\n";
     printf "    *!      multiplication (all values)\n";
     printf "    /       division\n";
     printf "    %%       remainder\n";
     printf "    **      power\n";
     printf "    !       negation\n");
  ("ls", "show values in the stack",
   fun eval ->
     ignore @@ List.fold_left
                 (fun i v ->
                    printf "%d: %s\n" i (Value.to_string v);
                    i - 1)
                 ((List.length eval.stack) - 1)
                 eval.stack);
  ("pop", "pop the top of the stack", fun eval -> ignore @@ pop eval);
  ("top", "show the top of the stack", fun eval ->
     match top eval with
     | None -> printf "error: stack is empty\n"
     | Some v -> printf "%s\n" (Value.to_string v));
]

let int_power x y =
  let rec f accu y =
    if y > 0 then f (accu*x) (y-1) else accu
  in
  f x (y-1)

let run eval s =
  let f node =
    let open Ast in
    match node.loc_desc with
    | Int v -> push eval (Value.Int v)
    | Float v -> push eval (Value.Float v)
    | Symbol v -> push eval (Value.Symbol v)
    | Add -> arith_op eval (+) (+.)
    | Sub -> arith_op eval (-) (-.)
    | Mul -> arith_op eval ( * ) ( *. )
    | Div -> arith_op eval (/) (/.)
    | Pow -> arith_op eval int_power power
    | Rem -> arith_op eval (mod) mod_float
    | Add_all -> arith_all_op eval (+) (+.)
    | Sub_all -> arith_all_op eval (-) (-.)
    | Mul_all -> arith_all_op eval ( * ) ( *. )
    | Bang ->
      let v = match pop_exn eval with
        | Value.Int x -> Value.Int (-x)
        | Value.Float x -> Value.Float (-.x)
        | _ -> error "invalid value"
      in
      push eval v
    | Command name ->
      begin match List.find_map_opt
                    (fun (cmd, _, f) ->
                       if cmd = name then Some f else None) commands
      with
      | None -> error (sprintf "unknown command: %s" name)
      | Some f -> f eval
      end
  in

  let lexbuf = Lexing.from_string s in
  try begin
     let nodes = Parser.command Lexer.read lexbuf in
     List.iter f nodes
  end with
  | Parser.Error -> raise (Error (Some lexbuf.lex_start_pos, "invalid syntax"))
  | exn -> raise exn
