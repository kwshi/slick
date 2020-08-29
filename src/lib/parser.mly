%{
open Containers

module Expr = Ast.Expr.Untyped

module Slick = struct end

%}

%token <string> LOWER_IDENT
%token <string> UPPER_IDENT
%token <Z.t> INT
%token BACKSLASH
%token HYPHEN
%token LPAREN
%token RPAREN
%token ARROW
%token EQUALS
%token LBRACE
%token RBRACE
%token SEMICOLON
%token COLON
%token COMMA
%token DOT
%token PIPE
%token WALRUS
%token EOF

%start <Ast.Expr.Untyped.t> prog

%%

prog:
  | e = expr; EOF { e }

(*
type_:
  | r = record_type { Ast.Type.Record r }
  | f = function_type { Ast.Type.Function f }

function_type:
  | r = record_type; ARROW; t = type_ { (r, t) }

*)

expr:
  | v = LOWER_IDENT; WALRUS; e = expr; SEMICOLON; b = expr { Expr.make_assign v e b }
  | f = function_expr { f }
  | a = function_app { a }
  | s = UPPER_IDENT; r = record_expr { Expr.make_variant s r }
  | e = atomic_expr { e }
  | n = INT { Expr.(make_literal (Int n)) }

atomic_expr:
  | LPAREN; e = expr; RPAREN { e }
  | r = record_expr { Expr.make_record r }
  | v = LOWER_IDENT { Expr.make_var v }
  | r = atomic_expr; DOT; l = LOWER_IDENT { Expr.make_projection r l }
  | LBRACE; e = expr; PIPE; l = comma_sequence(record_expr_entry) RBRACE { Expr.make_extensions e l }

rev_comma_sequence(item):
  | { [] }
  | i = item { [i] }
  | items = rev_comma_sequence(item); COMMA; i = item { i :: items }

comma_sequence(item):
  | rev_items = rev_comma_sequence(item) { List.rev rev_items }

brace_list(entry):
  | LBRACE; vs = comma_sequence(entry); RBRACE { vs }

(* 
record_type:
  | l = brace_list(record_type_entry) { l }

record_type_entry:
  | k = LOWER_IDENT; COLON; t = type_ { (k, t) }

*)

record_expr:
  | l = brace_list(record_expr_entry) { l }

record_expr_entry:
  | k = LOWER_IDENT; EQUALS; v = expr { (k, v) }

function_expr:
  | BACKSLASH; v = LOWER_IDENT; ARROW; e = expr { Expr.make_function v e }

function_app:
  | f = atomic_expr; e = atomic_expr { Expr.make_application f e }

