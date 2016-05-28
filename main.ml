open Printf

let version = "1.0"

let anon_fun _ = ()

let usage = "Usage: ocalc [options]"

let options = [
  ("-version", Arg.Unit (fun () -> printf "%s\n" version), "Print version")
]

(* entry point *)
let _ =
  Arg.parse options anon_fun usage;
  while true do
    let lexbuf = ref @@ Lexing.from_string "" in (* dummy *)
    try begin
      printf "> ";
      let line = String.trim @@ read_line () in
      lexbuf := Lexing.from_string line;
      let _ = Parser.command Lexer.read !lexbuf in
      ()
    end with
    | Parser.Error -> printf "%s\n" (Lexer.parse_error !lexbuf "invalid syntax")
    | exn -> raise exn
  done

(*val command: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.t list)*)

