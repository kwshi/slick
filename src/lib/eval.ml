open Containers


let rec evaluate (sc : Val.t Scope.t) expr =
  match expr.Ast.Expr.expr with
  | Assign (v, e, b) ->
    evaluate (Scope.add v (evaluate sc e) sc) b
  | Function (v, e) ->
    Function (fun value -> evaluate (Scope.add v value sc) e)
  | Application (f, e) ->
    (match evaluate sc f with
     | Function f' -> f' (evaluate sc e)
     | _ -> assert false
    )
  | Record r ->
    Record (r |> List.map @@ Pair.map2 @@ evaluate sc)
  | Projection (r, lbl) ->
    (match evaluate sc r with
     | Record r' -> snd @@ List.find (fun (lbl', _) -> String.(equal lbl lbl')) r'
     | _ -> assert false
    )
  | Extension (lbl, e, r) ->
    (match evaluate sc r with
     | Record r' -> Record ((lbl, evaluate sc e) :: r')
     | _ -> assert false
    )
  | Variant (v, r) ->
    Variant (v, r |> List.map @@ Pair.map2 @@ evaluate sc)
  | Var v ->
    Scope.find v sc
  | Literal l ->
    Primitive
      (match l with
       | Int n -> Int n
      )
