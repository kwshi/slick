open Containers
open Fun

let rec evaluate (sc : Val.t Scope.t) expr =
  match expr.Ast.Expr.expr with
  | Assign (v, e, b) ->
      evaluate (Scope.add v (evaluate sc e) sc) b
  | Function (v, e) ->
      Function (fun value -> evaluate (Scope.add v value sc) e)
  | Application (f, e) -> (
    match evaluate sc f with
    | Function f' ->
        f' (evaluate sc e)
    | _ ->
        assert false )
  | Record r ->
      Record (r |> List.map @@ Pair.map2 @@ evaluate sc)
  | Projection (r, lbl) -> (
    match evaluate sc r with
    | Record r' ->
        snd @@ List.find (fun (lbl', _) -> String.(equal lbl lbl')) r'
    | _ ->
        assert false )
  | Extension (lbl, e, r) -> (
    match evaluate sc r with
    | Record r' ->
        Record ((lbl, evaluate sc e) :: r')
    | _ ->
        assert false )
  | Variant (v, e) ->
      Variant (v, evaluate sc e)
  | Var v ->
      Scope.find v sc
  | Literal l ->
      Primitive (match l with Int n -> Int n)
  | Case (e, cs) ->
    (match evaluate sc e with
    | Variant (v, e) ->
      let _, p, b = List.find (fun (c, _, _) -> String.equal v c) cs in
      evaluate (Scope.add p e sc) b
    | _ -> assert false
    )
