{
open Lexing
open Parser

exception Error of (int * string)

let parse_error lexbuf msg =
    raise @@ Error (lexbuf.lex_start_pos, msg)

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
let symbol = '#' id

rule read =
  parse
  | space       { read lexbuf }
  | int
  { try
      INT (Ast.with_loc lexbuf.lex_start_pos (int_of_string (lexeme lexbuf)))
    with
    | Failure _ -> parse_error lexbuf "invalid integer value"
  }
  | float
  { try
      FLOAT (Ast.with_loc lexbuf.lex_start_pos (float_of_string (lexeme lexbuf)))
    with
    | Failure _ -> parse_error lexbuf "invalid float value"
  }
  | '+'         { ADD lexbuf.lex_start_pos }
  | '-'         { SUB lexbuf.lex_start_pos }
  | '*'         { MUL lexbuf.lex_start_pos }
  | '^'         { POW lexbuf.lex_start_pos }
  | '/'         { DIV lexbuf.lex_start_pos }
  | '%'         { REM lexbuf.lex_start_pos }
  | '!'         { BANG lexbuf.lex_start_pos }
  | id          { IDENT (Ast.with_loc lexbuf.lex_start_pos (lexeme lexbuf)) }
  | '#' (id as name)
  { SYMBOL (Ast.with_loc lexbuf.lex_start_pos name) }
  | _           { parse_error lexbuf "invalid syntax" }
  | eof         { EOL }
