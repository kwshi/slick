open Containers
open Fun

let prelude =
  let parse_string = Lexing.from_string %> Parser.prog Lexer.read in
  [ ("fix", {|\f -> (\x -> f (\v -> x x v)) (\x -> f (\v -> x x v))|})
    (*& "or"
      , {|\a -> \b -> case a | True -> a | False -> (case b | True -> b | False -> b)|}
    *)
    (*; "euler"
          , {|1
      |}
    *)
    (* dirty type hacks *)
    (*
   "some"
    , {|\a -> (\x -> case x | Some _ -> x | None -> x) (Some a)|}
  ; "none"
    , {|(\x -> case x | Some _ -> x | None -> x) None|}
       *)
  ]
  |> List.map (Pair.map2 parse_string)
  |> Module.eval
