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

      1
#{ id = 
#    fix (\id -> \l -> 
#      case l
#      | Nil -> l
#      | Cons {hd, tl} -> l
#    )
#}

|}
      (*
  ; "list"
    , {|
      {
      
id = fix (\id -> \l ->
  case l
  | Nil -> l
  | Cons{hd, tl} -> {_=id tl, l}.l
)
 
#map = fix (\map -> \f -> \l ->
#  x := (case l
#  | Nil -> Nil
#  | Cons{hd, tl} -> Cons{hd=f hd, tl=map f tl}
#      ); x
#      )
##case x
##
##| Nil -> x
##| Cons _ -> x
#}

|}
  (* dirty type hacks *)
  ; "true"
, {|(\x -> case x | True -> x | False -> x) True |}
  ; "false"
  , {|(\x -> case x | True -> x | False -> x) False|}
  ; "some"
    , {|\a -> (\x -> case x | Some _ -> x | None -> x) (Some a)|}
  ; "none"
    , {|(\x -> case x | Some _ -> x | None -> x) None|}
         *)
  ] 
  |> List.map (Pair.map2 parse_string)
  |> Module.eval
