%{
%}

%token <string Ast.loc> IDENT
%token <string Ast.loc> SYMBOL
%token <int Ast.loc> INT
%token <float Ast.loc> FLOAT
%token <int> ADD        (* "+" *)
%token <int> ADD_ALL    (* "+!" *)
%token <int> SUB        (* "-" *)
%token <int> SUB_ALL    (* "-!" *)
%token <int> MUL        (* "*" *)
%token <int> MUL_ALL    (* "*!" *)
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
  | ADD_ALL { Ast.with_loc $1 Ast.Add_all }
  | SUB { Ast.with_loc $1 Ast.Sub }
  | SUB_ALL { Ast.with_loc $1 Ast.Sub_all }
  | MUL { Ast.with_loc $1 Ast.Mul }
  | MUL_ALL { Ast.with_loc $1 Ast.Mul_all }
  | DIV { Ast.with_loc $1 Ast.Div }
  | REM { Ast.with_loc $1 Ast.Rem }
  | POW { Ast.with_loc $1 Ast.Pow }
  | BANG { Ast.with_loc $1 Ast.Bang }
