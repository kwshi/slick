%{
open Containers

module Expr = Slick_ast.Expr.Untyped

module Slick = struct end

%}

%token <string> LOWER_IDENT UPPER_IDENT
%token <Z.t> INT
%token <string> STRING
%token BACKSLASH
%token CASE
%token DEF
%token LPAREN RPAREN
%token LBRACE RBRACE
%token ARROW
%token EQUALS
%token SEMICOLON COLON COMMA DOT
%token PIPE
%token WALRUS
%token EOF

%token PLUS MINUS
%token PLUS_PLUS
%token ASTERISK SLASH MOD
%token GE GT LE LT EQ NE
%token AND OR
%token POW

%left OR
%left AND

%left PLUS_PLUS
%left PLUS MINUS
%left ASTERISK SLASH MOD
%right POW

%start <Slick_ast.Expr.Untyped.t> prog
%start <unit Slick_ast.Module.t> module_
%start <unit Slick_ast.Repl.t> repl

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
  | DEF; s = LOWER_IDENT; args = rev_def_args; COLON; e = expr { (s, Expr.make_function_curried (List.rev args) e) }


rev_def_args:
  | { [] }
  | args = rev_def_args; a = pattern_atom { a :: args }

repl:
  | EOF { Slick_ast.Repl.Empty }
  | e = expr; EOF { Slick_ast.Repl.Expr e }
  | e = module_entry; EOF { let s, e = e in Slick_ast.Repl.Def (s, e) }
  | s = LOWER_IDENT; WALRUS; e = expr; EOF { Slick_ast.Repl.Def (s, e) }
  | COLON; c = LOWER_IDENT; s = STRING { Slick_ast.Repl.Cmd (c, s) }

pattern:
  | lbl = UPPER_IDENT; p = pattern_atom { Slick_ast.Pattern.Variant (lbl, p)}
  | p = pattern_atom { p }

pattern_atom:
  | v = UPPER_IDENT { Slick_ast.Pattern.Variant (v, Slick_ast.Pattern.Record []) }
  | p = record_pattern { p }
  | p = literal_pattern { p }
  | v = LOWER_IDENT { Slick_ast.Pattern.Var v }
  | LPAREN; p = pattern; RPAREN { p }

expr:
  | v = LOWER_IDENT; WALRUS; e = expr_body; SEMICOLON; b = expr { Expr.make_assign v e b }
  | f = function_expr { f }
  | CASE; e = expr_body; COLON; l = rev_case_entries { Expr.make_case e (List.rev l) }
  | e = expr_body { e }

expr_body:
  | e = expr_bool_bop { e }

expr_bool_bop:
  | a = expr_bool_bop; o = bool_bop; b = expr_bool_bop { Expr.make_bop o a b }
  | e = expr_comp { e }

%inline bool_bop:
  | AND { "&&" }
  | OR { "||" }

expr_comp:
  | a = expr_bop; c = comp; b = expr_bop { Expr.make_bop c a b }
  | e = expr_bop { e }

comp:
  | LT { "<" }
  | LE { "<=" }
  | GT { ">" }
  | GE { ">=" }
  | EQ { "==" }
  | NE { "!=" }

expr_bop:
  | a = expr_bop; o = bop; b = expr_bop { Expr.make_bop o a b }
  | e = expr_op_neg { e }

%inline bop:
  | PLUS { "+" }
  | MINUS { "-" }
  | ASTERISK { "*" }
  | SLASH { "/" }
  | MOD { "%" }
  | POW { "**" }
  | PLUS_PLUS { "++" }

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
  | v = LOWER_IDENT { Expr.make_var v }
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

brace_list(entry):
  | LBRACE; vs = comma_sequence(entry); RBRACE { vs }

record_expr:
  | l = brace_list(record_expr_entry) { l }

record_expr_entry:
  | k = LOWER_IDENT; EQUALS; v = expr { (k, v) }
  | k = LOWER_IDENT { (k, Expr.make_var k) }

record_pattern:
  | l = brace_list(record_pattern_entry) { Slick_ast.Pattern.Record l }

record_pattern_entry:
  | k = LOWER_IDENT { (k, Slick_ast.Pattern.Var k) }
  | k = LOWER_IDENT; EQUALS; p = pattern; { (k, p) }
  (* No need for parens inside of a record *)


literal_pattern:
  | n = STRING { Slick_ast.Pattern.Literal (String n)}
  | i = INT { Slick_ast.Pattern.Literal (Int i)}

function_expr:
  | BACKSLASH; p = pattern; ARROW; e = expr { Expr.make_function p e }
