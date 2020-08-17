open Containers
    
let () =
  while true do
    Lexing.from_string (read_line ())
    |> Slick__Parser.prog Slick__Lexer.read 
    |> Slick__Eval.evaluate {Slick__Eval.lookup_table = Slick__Eval.LookupTable.empty}
    |> Slick__Eval.pp_value Format.stdout
    ; Format.newline Format.stdout ()
    ; Format.flush Format.stdout ()
  done
