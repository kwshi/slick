%{

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

let prog := ~ = expr; EOF; <>

let module_ := ~ = list(module_entry); EOF; <>

(*
type_:
  | r = record_type { Ast.Type.Record r }
  | f = function_type { Ast.Type.Function f }

function_type:
  | r = record_type; ARROW; t = type_ { (r, t) }

*)


let module_entry :=
  | DEF; s = LOWER_IDENT; args = list(pattern_atom); COLON; e = expr; { (s, Expr.make_function_curried args e) }


let repl :=
  | EOF; {Slick_ast.Repl.Empty}
  | ~ = expr; EOF; <Slick_ast.Repl.Expr>
  | (~, ~) = module_entry; EOF; <Slick_ast.Repl.Def>
  | ~ = LOWER_IDENT; WALRUS; ~ = expr; EOF; <Slick_ast.Repl.Def>
  | COLON; ~ = LOWER_IDENT; ~ = STRING; <Slick_ast.Repl.Cmd>

let pattern :=
  | ~ = UPPER_IDENT; ~ = pattern_atom; <Slick_ast.Pattern.Variant>
  | ~ = pattern_atom; <>

let pattern_atom :=
  | v = UPPER_IDENT; { Slick_ast.Pattern.Variant (v, Slick_ast.Pattern.Record []) }
  | ~ = record_pattern; <>
  | ~ = literal_pattern; <Slick_ast.Pattern.Literal>
  | ~ = LOWER_IDENT; <Slick_ast.Pattern.Var>
  | LPAREN; ~ = pattern; RPAREN; <>

let expr :=
  | v = LOWER_IDENT; WALRUS; e = expr_body; SEMICOLON; b = expr; { Expr.make_assign v e b }
  | ~ = function_expr; <>
  | CASE; e = expr_body; COLON; l = nonempty_list(case_entry); { Expr.make_case e l }
  | ~ = expr_body; <>

let expr_body :=
  | ~ = expr_bool_bop; <>

let expr_bool_bop :=
  | a = expr_bool_bop; o = bool_bop; b = expr_bool_bop; { Expr.make_bop o a b }
  | ~ = expr_comp; <>

let bool_bop ==
  | AND; { "&&" }
  | OR; { "||" }

let expr_comp :=
  | a = expr_bop; c = comp; b = expr_bop; { Expr.make_bop c a b }
  | ~ = expr_bop; <>

let comp :=
  | LT; { "<" }
  | LE; { "<=" }
  | GT; { ">" }
  | GE; { ">=" }
  | EQ; { "==" }
  | NE; { "!=" }

let expr_bop :=
  | a = expr_bop; o = bop; b = expr_bop; { Expr.make_bop o a b }
  | ~ = expr_uop; <>

let bop ==
  | PLUS; { "+" }
  | MINUS; { "-" }
  | ASTERISK; { "*" }
  | SLASH; { "/" }
  | MOD; { "%" }
  | POW; { "**" }
  | PLUS_PLUS; { "++" }

let expr_uop :=
  | o = uop; e = expr_app; { Expr.make_uop o e }
  | ~ = expr_app; <>
  | ~ = expr_variant; <>

let uop ==
  | MINUS; { "$-" }

let expr_variant :=
  | s = UPPER_IDENT; e = expr_atom; { Expr.make_variant s e }
  | s = UPPER_IDENT; e = expr_variant_atom; { Expr.make_variant s e }
  | ~ = expr_variant_atom; <>

let expr_app :=
  | f = expr_app; e = expr_atom; { Expr.make_application f e }
  | f = expr_app; e = expr_variant_atom; { Expr.make_application f e }
  | ~ = expr_atom; <>

let expr_atom :=
  | ~ = record_expr; <Expr.make_record>
  | ~ = LOWER_IDENT; <Expr.make_var>
  | r = expr_atom; DOT; l = LOWER_IDENT; { Expr.make_projection r l }
  | ~ = expr_lit; <Expr.make_literal>
  | LBRACE; e = expr_body; PIPE; l = separated_nonempty_list(COMMA, record_expr_entry); RBRACE; { Expr.make_extensions e l }
  | LPAREN; ~ = expr; RPAREN; <>

let expr_lit :=
  | ~ = STRING; <Slick_ast.Expr.String>
  | ~ = INT; <Slick_ast.Expr.Int>

let expr_variant_atom ==
  | s = UPPER_IDENT; { Expr.make_variant s (Expr.make_record []) }

let case_entry ==
  | PIPE; ~ = pattern; ARROW; ~ = expr_body; <>

let record_expr ==
  | LBRACE; ~ = separated_list(COMMA, record_expr_entry); RBRACE; <>

let record_expr_entry :=
  | ~ = LOWER_IDENT; EQUALS; ~ = expr; <>
  | k = LOWER_IDENT; { (k, Expr.make_var k) }

let record_pattern ==
  | LBRACE; ~ = separated_list(COMMA, record_pattern_entry); RBRACE; <Slick_ast.Pattern.Record>

let record_pattern_entry :=
  | k = LOWER_IDENT; { (k, Slick_ast.Pattern.Var k) }
  | ~ = LOWER_IDENT; EQUALS; ~ = pattern; <>

let literal_pattern :=
  | ~ = STRING; <Slick_ast.Pattern.String>
  | ~ = INT;    <Slick_ast.Pattern.Int>

let function_expr ==
  | BACKSLASH; p = pattern; ARROW; e = expr; { Expr.make_function p e }
