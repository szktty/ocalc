%{
%}

%token <string Ast.loc> IDENT
%token <int Ast.loc> INT
%token <float Ast.loc> FLOAT
%token <int> LPAREN
%token <int> RPAREN
%token <int> DOT        (* "." *)
%token <int> ADD        (* "+" *)
%token <int> SUB        (* "-" *)
%token <int> MUL        (* "*" *)
%token <int> DIV        (* "/" *)
%token <int> REM        (* "%" *)
%token <int> POW        (* "^" *)
%token <int> CLEAR      (* "ac" *)
%token <int> READ       (* "r" *)
%token <int> WRITE      (* "w" *)
%token <int> SWAP       (* "sw" *)
%token <int> LIST       (* "l" *)
%token <int> ROLLDOWN   (* "rd" *)
%token <int> ROLLUP     (* "ru" *)
%token <int> ABS        (* "abs" *)
%token EOL

%start <Ast.t list> command

%%

command:
  | term* EOL { $1 }

term:
  | INT { Ast.of_loc $1 (Ast.Int $1.loc_desc) }
  | FLOAT { Ast.of_loc $1 (Ast.Float $1.loc_desc) }

