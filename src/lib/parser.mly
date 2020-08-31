%{
open Containers

module Expr = Ast.Expr.Untyped

module Slick = struct end

%}

%token <string> LOWER_IDENT
%token <string> UPPER_IDENT
%token <Z.t> INT
%token <string> STRING
%token BACKSLASH
%token CASE
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

(* TODO reverse *)

expr:
  | v = LOWER_IDENT; WALRUS; e = expr_body { Expr.make_assign v e }
  | f = function_expr { f }
  | CASE; e = expr_body; COLON; l = rev_case_entries { Expr.make_case e (List.rev l) }
  | e = expr_body { e }

expr_body:
  | e = expr_line { e }
  | e1 = expr_line; SEMICOLON; e2 = expr_body { Expr.make_sequence e1 e2 }

expr_line:
  | e = expr_apps { e }
  | s = UPPER_IDENT; e = expr_atom { Expr.make_variant s e }

expr_apps:
  | f = expr_apps; e = expr_atom { Expr.make_application f e }
  | e = expr_atom { e }

expr_atom:
  | LPAREN; e = expr; RPAREN { e }
  | r = record_expr { Expr.make_record r }
  | v = LOWER_IDENT { Expr.make_var v }
  | r = expr_atom; DOT; l = LOWER_IDENT { Expr.make_projection r l }
  | n = INT { Expr.(make_literal (Int n)) }
  | s = STRING { Expr.(make_literal (String s))}
  | LBRACE; e = expr_body; PIPE; l = comma_sequence(record_expr_entry) RBRACE { Expr.make_extensions e l }

rev_case_entries:
  | e = case_entry { [e] }
  | l = rev_case_entries; e = case_entry { e :: l }

case_entry:
  | PIPE; lbl = UPPER_IDENT; var = LOWER_IDENT; ARROW; e = expr_body { (Ast.Expr.Tag_pat (lbl, var), e) }
  | PIPE; var = LOWER_IDENT; ARROW; e = expr_body { (Ast.Expr.Var_pat var, e) }

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

