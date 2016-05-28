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
    printf "> ";
    let line = String.trim @@ read_line () in
    let lexbuf = Lexing.from_string line in
    let _ = Parser.command Lexer.read lexbuf in
    ()
  done

(*val command: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.t list)*)

