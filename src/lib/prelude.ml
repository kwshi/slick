open Containers
open Fun

let prelude =
  let parse_string  =
    Lexing.from_string
    %> Parser.prog Lexer.read
  in
  [ "fix"
  , {|\f -> (\x -> f (\v -> x x v)) (\x -> f (\v -> x x v))|}
  ; "list"
    , {|
      {
nil = Nil,
cons = \{a, b} -> Cons{hd=a, tl=b}
,
 
map = fix (\map -> \f -> \l ->
  case l
  | Nil -> Nil
  | Cons {} -> Cons
      )
      

    

}
|}
  ] 
  |> List.map (Pair.map2 parse_string)
  |> Module.eval
