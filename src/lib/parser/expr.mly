%%

%public let expr :=
  | v = LOWER_IDENT; WALRUS; e = expr_body; SEMICOLON; b = expr; { Expr.make_assign v e b }
  | BACKSLASH; p = pattern; ARROW; e = expr; { Expr.make_function p e }
  | CASE; e = expr_body; COLON; l = list_min1(case_entry); { Expr.make_case e l }
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
  | SLASH_SLASH; { "//" }
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
  (*| LPAREN; (pos, kv) = tuple_entries; RPAREN; {Expr.make_tuple pos kv}*)
  | ~ = LOWER_IDENT; <Expr.make_var>
  | r = expr_atom; DOT; l = LOWER_IDENT; { Expr.make_projection r l }
  | ~ = expr_lit; <Expr.make_literal>
  | LBRACE; e = expr_body; PIPE; l = list_sep_min1(COMMA, tuple_named_entry); RBRACE; { Expr.make_extensions e l }
  | LPAREN; ~ = expr_paren; RPAREN; <>

let expr_paren :=
  | es = tuple_pos_entries_trail; {Expr.make_tuple (List.rev es) []}
  | es = tuple_pos_entries_notrail;
    {match es with
     | [e] -> e
     | _ -> Expr.make_tuple (List.rev es) []
    }
  | es = tuple_named_entries_notrail; COMMA?; {Expr.make_tuple [] (List.rev es)}
  | (pos, kv) = tuple_both_entries_notrail; COMMA?; {Expr.make_tuple (List.rev pos) (List.rev kv)}

let tuple_pos_entries_trail :=
  | {[]}
  | ~ = tuple_pos_entries_notrail; COMMA; <>

let tuple_pos_entries_notrail :=
  | es = tuple_pos_entries_trail; e = expr; {e :: es}
  (*| e = expr; {[e]}
  | es = tuple_pos_entries_notrail; COMMA; e = expr; {e :: es}
*)

let tuple_named_entries_notrail :=
  | e = tuple_named_entry; {[e]}
  | es = tuple_named_entries_notrail; COMMA; e = tuple_named_entry; {e :: es}

let tuple_both_entries_notrail :=
  | pos = tuple_pos_entries_notrail; COMMA; e = tuple_named_entry; {pos, [e]}
  | (pos, kv) = tuple_both_entries_notrail; COMMA; e = tuple_named_entry; {pos, e :: kv}


(*
let tuple_rev_entries :=
  | e = expr; {[e]}
  | es = tuple_rev_entries; COMMA; e = expr; {e :: es}

let tuple_rev_entries_min2 :=
  | e1 = expr; COMMA; e2 = expr; {[e1; e2]}
  | es = tuple_rev_entries_min2; COMMA; e = expr; {e :: es}
*)


(*
let tuple_pos_entries_min1_rev :=
  | p = expr_body; {[p]}
  | ps = tuple_pos_entries_min1_rev; COMMA; p = expr_body; {p :: ps}

let tuple_pos_entries_min2_rev :=
  | p1 = expr_body; p2 = expr_body; {[p; q]}
  | ps = tuple_pos_entries_min2_rev; COMMA; p = expr_body; {p :: ps}

let tuple_entries_rev := 
  | ps = tuple_pos_entries_min2_rev; {ps, []}
  | ps = tuple_pos_entries_min1_rev; {ps, []}
  | kv = tuple_named_entry; {[], [kv]}
  | tuple_entries_rev

*)

(*
let tuple_entries :=
  | {[], []}
  | p = expr_body; COMMA; {[p], []}
  | pos = list_sep_min2(COMMA, expr_body); COMMA?; {pos, []}
  | kv = list_sep_min1(COMMA, tuple_named_entry); COMMA?; {[], kv}
  | ~ = list_sep_min1(COMMA, expr_body); COMMA;
    ~ = list_sep_min1(COMMA, tuple_named_entry); COMMA?; <>
*)
(*
  | kv = separated_nonempty_list(COMMA, tuple_named_entry); COMMA?; {[], kv}
  | ~ = separated_nonempty_list(COMMA, expr_body); COMMA;
    ~ = separated_nonempty_list(COMMA, tuple_named_entry); <>

let rev_nl(entry) :=
  | e = entry; {[e]}
  | es = rev_nl(entry); e = entry; {e :: es}

let nl(entry) ==
  | ~ = rev_nl(entry); <List.rev>

*)
let expr_lit :=
  | ~ = STRING; <Ast.Expr.String>
  | ~ = INT; <Ast.Expr.Int>

let expr_variant_atom ==
  | s = UPPER_IDENT; { Expr.make_variant s (Expr.make_record []) }

let case_entry ==
  | PIPE; ~ = pattern; ARROW; ~ = expr_body; <>

let tuple_named_entry ==
  | ~ = LOWER_IDENT; EQUALS; ~ = expr; <>

