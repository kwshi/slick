let () =
  while true do
    Lexing.from_string (read_line ())
    |> Slick.Parser.prog Slick.Lexer.read
    |> Slick.Ast.show_expr
    |> print_endline
  done
