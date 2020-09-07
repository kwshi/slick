%{
%}

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
  | DEF; s = LOWER_IDENT; args = list(pattern_atom); COLON; e = expr;
    { (s, Ast.Expr.Untyped.make_function_curried args e) }

let repl :=
  | EOF; {Ast.Repl.Empty}
  | ~ = expr; EOF; <Ast.Repl.Expr>
  | (~, ~) = module_entry; EOF; <Ast.Repl.Def>
  | ~ = LOWER_IDENT; WALRUS; ~ = expr; EOF; <Ast.Repl.Def>
  | COLON; ~ = LOWER_IDENT; ~ = STRING; <Ast.Repl.Cmd>

