%{
open Containers
%}

%token <string> LOWER_IDENT
%token <string> UPPER_IDENT
%token BACKSLASH
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

type_:
  | r = record_type { Ast.Type_record r }
  | f = function_type { Ast.Type_function f }

function_type:
  | r = record_type; ARROW; t = type_ { (r, t) }

expr:
  | f = function_expr { Ast.Function f }
  | a = function_app { Ast.Application a }
  | e = atomic_expr { e }

atomic_expr:
  | LPAREN; e = expr; RPAREN { e }
  | r = record_expr { Ast.Record r }
  | s = UPPER_IDENT; r = record_expr { Ast.Variant (s, r) }

rev_comma_sequence(item):
  | { [] }
  | i = item { [i] }
  | items = rev_comma_sequence(item); COMMA; i = item { i :: items }

comma_sequence(item):
  | rev_items = rev_comma_sequence(item) { List.rev rev_items }

brace_list(entry):
  | LBRACE; vs = comma_sequence(entry); RBRACE { vs }

record_type:
  | l = brace_list(record_type_entry) { l }

record_type_entry:
  | k = LOWER_IDENT; COLON; t = type_ { (k, t) }

record_expr:
  | l = brace_list(record_expr_entry) { l }

record_expr_entry:
  | k = LOWER_IDENT; EQUALS; v = expr { (k, v) }

function_expr:
  | BACKSLASH; r = record_type; ARROW; e = expr { (r, e) }

function_app:
  | f = atomic_expr; e = atomic_expr { (f, e) }

