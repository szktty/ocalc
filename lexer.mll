{
open Lexing
open Parser

exception Error of string

let parse_error lexbuf msg =
    let buf = Buffer.create 64 in
    let s = lexeme lexbuf in
    Buffer.add_string buf s;
    Buffer.add_string buf "\n";
    for _ = 0 to (lexbuf.lex_start_pos-1) do
        Buffer.add_string buf " "
    done;
    Buffer.add_string buf "^\n";
    Buffer.add_string buf "error: ";
    Buffer.add_string buf msg;
    Buffer.contents buf

let raise_parse_error lexbuf msg =
    raise @@ Error (parse_error lexbuf msg)

}

let hex = '0' ['x' 'X']
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let int = digit+ | hex hexdigit+
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let fnum = digit+ '.' digit* | ['.']? digit+
let hexfnum = hexdigit+ '.' hexdigit* | ['.']? hexdigit+
let binexp = ['p' 'P'] ['-' '+']? digit+
let float = fnum exp? | hex hexfnum binexp?
let space = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | space       { read lexbuf }
  | int
  { try
      INT (Ast.with_loc lexbuf.lex_start_pos (int_of_string (lexeme lexbuf)))
    with
    | Failure _ -> raise_parse_error lexbuf "invalid integer value"
  }
  | float
  { try
      FLOAT (Ast.with_loc lexbuf.lex_start_pos (float_of_string (lexeme lexbuf)))
    with
    | Failure _ -> raise_parse_error lexbuf "invalid float value"
  }
  | '('         { LPAREN lexbuf.lex_start_pos }
  | ')'         { RPAREN lexbuf.lex_start_pos }
  | '.'         { DOT lexbuf.lex_start_pos }
  | '+'         { ADD lexbuf.lex_start_pos }
  | '-'         { SUB lexbuf.lex_start_pos }
  | '*'         { MUL lexbuf.lex_start_pos }
  | '^'         { POW lexbuf.lex_start_pos }
  | '/'         { DIV lexbuf.lex_start_pos }
  | '%'         { REM lexbuf.lex_start_pos }
  | id          { IDENT (Ast.with_loc lexbuf.lex_start_pos (lexeme lexbuf)) }
  | _           { raise_parse_error lexbuf "invalid syntax" }
  | eof         { EOL }
