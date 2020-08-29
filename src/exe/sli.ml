open Containers

let () =
  while true do
    let expr, _ =
      LNoise.linenoise "sli> "
      |> Option.get_exn
      |> Lexing.from_string
      |> Slick.Parser.prog Slick.Lexer.read
      |> Slick.Typing.infer_top Slick.Typing.empty_ctx
    in
    Fmt.(hbox (any "type:@ " ++ Slick.Typing.pp)) Format.stdout expr.Slick.Ast.Expr.tp
  ; Format.pp_print_newline Format.stdout () 
  ; Slick.Eval.evaluate Slick.Eval.Scope.empty expr
    |> Fmt.(hbox (any "val:@ " ++ Slick.Eval.pp)) Format.stdout 
  ; Format.pp_print_newline Format.stdout () 
  done
