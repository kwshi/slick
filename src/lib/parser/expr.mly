%%

%public let expr :=
  | v = LOWER_IDENT; WALRUS; e = expr_body; SEMICOLON; b = expr; { Expr.make_assign v e b }
  | BACKSLASH; p = pattern; ARROW; e = expr; { Expr.make_function p e }
  | CASE; e = expr_body; COLON; l = nonempty_list(case_entry); { Expr.make_case e l }
  | ~ = expr_body; <>

let expr_body ==
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
  | LBRACE; ~ = separated_list(COMMA, record_expr_entry); RBRACE; <Expr.make_record>
  | ~ = LOWER_IDENT; <Expr.make_var>
  | r = expr_atom; DOT; l = LOWER_IDENT; { Expr.make_projection r l }
  | ~ = expr_lit; <Expr.make_literal>
  | LBRACE; e = expr_body; PIPE; l = separated_nonempty_list(COMMA, record_expr_entry); RBRACE; { Expr.make_extensions e l }
  | LPAREN; ~ = expr; RPAREN; <>

let expr_lit :=
  | ~ = STRING; <Ast.Expr.String>
  | ~ = INT; <Ast.Expr.Int>

let expr_variant_atom ==
  | s = UPPER_IDENT; { Expr.make_variant s (Expr.make_record []) }

let case_entry ==
  | PIPE; ~ = pattern; ARROW; ~ = expr_body; <>

let record_expr_entry :=
  | ~ = LOWER_IDENT; EQUALS; ~ = expr; <>
  | k = LOWER_IDENT; { (k, Expr.make_var k) }

