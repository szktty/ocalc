%{
%}

%token <string Ast.loc> IDENT
%token <string Ast.loc> SYMBOL
%token <int Ast.loc> INT
%token <float Ast.loc> FLOAT
%token <int> ADD        (* "+" *)
%token <int> SUB        (* "-" *)
%token <int> MUL        (* "*" *)
%token <int> DIV        (* "/" *)
%token <int> REM        (* "%" *)
%token <int> POW        (* "^" *)
%token <int> BANG       (* "!" *)
%token EOL

%start <Ast.t list> command

%%

command:
  | term* EOL { $1 }

term:
  | INT { Ast.of_loc $1 (Ast.Int $1.loc_desc) }
  | FLOAT { Ast.of_loc $1 (Ast.Float $1.loc_desc) }
  | IDENT { Ast.of_loc $1 (Ast.Command $1.loc_desc) }
  | SYMBOL { Ast.of_loc $1 (Ast.Symbol $1.loc_desc) }
  | ADD { Ast.with_loc $1 Ast.Add }
  | SUB { Ast.with_loc $1 Ast.Sub }
  | MUL { Ast.with_loc $1 Ast.Mul }
  | DIV { Ast.with_loc $1 Ast.Div }
  | REM { Ast.with_loc $1 Ast.Rem }
  | POW { Ast.with_loc $1 Ast.Pow }
  | BANG { Ast.with_loc $1 Ast.Bang }
