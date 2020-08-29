open Containers

let pp =
  let open Fmt in
  hvbox
    (pair ~sep:(sp ++ styled (`Fg `Blue) (hbox (any ":@ ")))
       (styled (`Fg (`Hi `Cyan)) Slick.Eval.pp)
       (styled (`Fg (`Hi `Blue)) @@ Slick.Typing.pp)
    )
  ++ const Format.pp_print_newline ()

let () =
  Fmt.(set_style_renderer stdout `Ansi_tty)
; while true do
    let input =
      LNoise.linenoise "slick> "
      |> Option.get_exn
    in
    LNoise.history_add input
    |> Result.get_exn
  ; let expr, _ =
      Lexing.from_string input
      |> Slick.Parser.prog Slick.Lexer.read
      |> Slick.Typing.infer_top Slick.Typing.empty_ctx
    in
    pp Fmt.stdout
      (Slick.Eval.evaluate Slick.Eval.Scope.empty expr,
       expr.Slick.Ast.Expr.tp)
  done
