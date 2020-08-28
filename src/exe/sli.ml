open Containers

let () =
  while true do
    let expr, _ =
      Fmt.(hbox @@ any "sli>@ @?") Format.stdout ()
    ; Lexing.from_string (read_line ())
      |> Slick.Parser.prog Slick.Lexer.read
      |> Slick.Typing.infer_top Slick.Typing.empty_ctx
    in
    Fmt.(hbox (any "type:@ " ++ Slick.Typing.pp)) Format.stdout expr.Slick.Ast.Expr.tp
  ; (*Slick.Eval.evaluate
      {Slick.Eval.lookup_table = Slick.Eval.LookupTable.empty}
      |> Fmt.(any "val:@ " ++ Slick.Ast.Eval.pp) Format.stdout ;*)
  ; Format.pp_print_newline Format.stdout () 
  done
