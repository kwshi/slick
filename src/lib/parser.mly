
%token <string> IDENT
%token LPAREN
%token RPAREN
%token ARROW
%token EQUALS
%token LBRACE
%token RBRACE
%token SEMICOLON
%token COLON
%token COMMA
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | f = function_expr { Ast.Function f }
  | a = function_app { Ast.Application a }
  | e = atomic_expr { e }

atomic_expr:
  | LPAREN; e = expr; RPAREN { e }
  | r = record_expr { Ast.Record r }

record_expr:
  | LBRACE; vs = record_expr_values; RBRACE { vs }

record_expr_values:
  | { [] }
  | v = record_expr_value { [v] }
  | v = record_expr_value; COMMA; vs = record_expr_values { v :: vs }

record_expr_value:
  | k = IDENT; EQUALS; v = expr { (k, v) }

function_expr:
  | r = record_expr; ARROW; e = expr { (r, e) }

function_app:
  | f = atomic_expr; e = atomic_expr { (f, e) }

