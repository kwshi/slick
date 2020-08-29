open Containers

let () =
  while true do
    let input =
      LNoise.linenoise "sli> "
      |> Option.get_exn
    in
    LNoise.history_add input
    |> Result.get_exn
  ; let expr, _ =
      Lexing.from_string input
      |> Slick.Parser.prog Slick.Lexer.read
      |> Slick.Typing.infer_top Slick.Typing.empty_ctx
    in
    Fmt.(hbox (any "type:@ " ++ Slick.Typing.pp)) Format.stdout expr.Slick.Ast.Expr.tp
  ; Format.pp_print_newline Format.stdout () 
  ; Slick.Eval.evaluate Slick.Eval.Scope.empty expr
    |> Fmt.(hbox (any "val:@ " ++ Slick.Eval.pp)) Format.stdout 
  ; Format.pp_print_newline Format.stdout () 
  done
