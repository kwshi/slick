%{
open Containers

module Expr = Ast.UntypedExpr

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

%start <Ast.UntypedExpr.t> prog

%%

prog:
  | e = expr; EOF { e }

type_:
  | r = record_type { Ast.Type_record r }
  | f = function_type { Ast.Type_function f }

function_type:
  | r = record_type; ARROW; t = type_ { (r, t) }

expr:
  | f = function_expr { f }
  | a = function_app { a }
  | s = UPPER_IDENT; r = record_expr { Expr.make_variant s r }
  | e = atomic_expr { e }

atomic_expr:
  | LPAREN; e = expr; RPAREN { e }
  | r = record_expr { Expr.make_record r }
  | v = LOWER_IDENT { Expr.make_var v }

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
  | BACKSLASH; r = record_type; ARROW; e = expr { Expr.make_function r e }

function_app:
  (* Should this be r = expr? *)
  | f = atomic_expr; r = atomic_expr { Expr.make_application f r }

