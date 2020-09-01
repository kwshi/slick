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
%token DEF
%token MINUS
%token PLUS
%token ASTERISK
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
%token SLASH
%token GE
%token GT
%token LE
%token LT
%token EQ
%token NE
%token WALRUS
%token EOF

%start <Ast.Expr.Untyped.t> prog
%start <unit Ast.Module.t> module_
%start <unit Ast.Repl.t> repl

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

module_:
  | m = module_entries; EOF { m }

module_entries:
  | m = rev_module_entries { List.rev m }

rev_module_entries:
  | { [] }
  | m = rev_module_entries; e = module_entry { e :: m }

module_entry:
  | DEF; s = LOWER_IDENT; args = def_args; COLON; e = expr { (s, Expr.make_function_with_args args e) }

def_args:
  | { [] }
  | LPAREN; args = non_empty_comma_sequence(pattern); RPAREN { args }

repl:
  | EOF { Ast.Repl.Empty }
  | e = expr; EOF { Ast.Repl.Expr e }
  | e = module_entry; EOF { let s, e = e in Ast.Repl.Def (s, e) }
  | s = LOWER_IDENT; WALRUS; e = expr; EOF { Ast.Repl.Def (s, e) }

pattern:
  | v = UPPER_IDENT; { Ast.Pattern.Variant (v, Ast.Pattern.Record []) }
  | LPAREN; p = variant_pattern; RPAREN { p }
  | p = record_pattern { p }
  | p = pattern_atom { p }

pattern_atom:
  | LPAREN; p = pattern; RPAREN { p }
  | p = literal_pattern { p }
  | v = LOWER_IDENT { Ast.Pattern.Var v }

expr:
  | v = LOWER_IDENT; WALRUS; e = expr_body; SEMICOLON; b = expr { Expr.make_assign v e b }
  | f = function_expr { f }
  | CASE; e = expr_body; l = rev_case_entries { Expr.make_case e (List.rev l) }
  | e = expr_body { e }

expr_body:
  | e = expr_op_comp { e }

expr_op_comp:
  | a = expr_op_add; LT; b = expr_op_add { Expr.make_bop "<" a b }
  | a = expr_op_add; LE; b = expr_op_add { Expr.make_bop "<=" a b }
  | a = expr_op_add; GT; b = expr_op_add { Expr.make_bop ">" a b }
  | a = expr_op_add; GE; b = expr_op_add { Expr.make_bop ">=" a b }
  | a = expr_op_add; EQ; b = expr_op_add { Expr.make_bop "==" a b }
  | a = expr_op_add; NE; b = expr_op_add { Expr.make_bop "!=" a b }
  | e = expr_op_add { e }

expr_op_add:
  | a = expr_op_add; PLUS; b = expr_op_mul { Expr.make_bop "+" a b }
  | a = expr_op_add; MINUS; b = expr_op_mul { Expr.make_bop "-" a b }
  | e = expr_op_mul { e }

expr_op_mul:
  | a = expr_op_mul; ASTERISK; b = expr_op_neg { Expr.make_bop "*" a b }
  | a = expr_op_mul; SLASH; b = expr_op_neg { Expr.make_bop "/" a b }
  | e = expr_op_neg { e }

expr_op_neg:
  | MINUS; e = expr_app { Expr.make_uop "$-" e }
  | e = expr_app { e }
  | e = expr_variant { e }

expr_variant:
  | s = UPPER_IDENT; e = expr_atom { Expr.make_variant s e }
  | s = UPPER_IDENT; e = expr_variant_atom { Expr.make_variant s e }
  | e = expr_variant_atom { e }

expr_app:
  | f = expr_app; e = expr_atom { Expr.make_application f e }
  | f = expr_app; e = expr_variant_atom { Expr.make_application f e }
  | e = expr_atom { e }

expr_atom:
  | LPAREN; e = expr; RPAREN { e }
  | r = record_expr { Expr.make_record r }
  | v = LOWER_IDENT { if String.equal v "_" then raise @@ Ast.SyntaxError "`_` cannot be used as variable" else Expr.make_var v }
  | r = expr_atom; DOT; l = LOWER_IDENT { Expr.make_projection r l }
  | n = INT { Expr.(make_literal (Int n)) }
  | s = STRING { Expr.(make_literal (String s))}
  | LBRACE; e = expr_body; PIPE; l = comma_sequence(record_expr_entry) RBRACE { Expr.make_extensions e l }

expr_variant_atom:
  | s = UPPER_IDENT { Expr.make_variant s (Expr.make_record []) }

rev_case_entries:
  | e = case_entry { [e] }
  | l = rev_case_entries; e = case_entry { e :: l }

case_entry:
  | PIPE; p = pattern; ARROW; e = expr_body { (p, e) }

rev_comma_sequence(item):
  | { [] }
  | i = item { [i] }
  | items = rev_comma_sequence(item); COMMA; i = item { i :: items }

comma_sequence(item):
  | rev_items = rev_comma_sequence(item) { List.rev rev_items }

non_empty_comma_sequence(item):
  | i = item; is = comma_sequence(item) { i :: is }

brace_list(entry):
  | LBRACE; vs = comma_sequence(entry); RBRACE { vs }

record_expr:
  | l = brace_list(record_expr_entry) { l }

record_expr_entry:
  | k = LOWER_IDENT; EQUALS; v = expr { (k, v) }

record_pattern:
  | l = brace_list(record_pattern_entry) { Ast.Pattern.Record l }

record_pattern_entry:
  | k = LOWER_IDENT { (k, Ast.Pattern.Var k) }
  | k = LOWER_IDENT; EQUALS; p = pattern; { (k, p) }
  (* No need for parens inside of a record *)
  | k = LOWER_IDENT; EQUALS; p = variant_pattern; { (k, p) }

variant_pattern:
  | lbl = UPPER_IDENT; p = pattern { Ast.Pattern.Variant (lbl, p)}

literal_pattern:
  | n = STRING { Ast.Pattern.Literal (String n)}
  | i = INT { Ast.Pattern.Literal (Int i)}

function_expr:
  | BACKSLASH; p = pattern; ARROW; e = expr { Expr.make_function p e }
