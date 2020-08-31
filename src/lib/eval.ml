open Containers
open Fun

let rec evaluate (sc : Val.t Scope.t) expr : Val.t * Val.t Scope.t =
  match expr.Ast.Expr.expr with
  | Assign (var, e) ->
    let v, _ = evaluate sc e in
    v, Scope.add var v sc

  | Function (v, e) ->
    Val.Function (fun value -> fst @@ evaluate (Scope.add v value sc) e),
    sc

  | Application (f, e) -> (
    match evaluate sc f with
    | Function f', _ ->
        f' (fst @@ evaluate sc e), sc
    | _ ->
        assert false )

  | Record r ->
    Record (r |> List.map @@ Pair.map2 @@ (evaluate sc %> fst)),
    sc

  | Projection (r, k) -> (
    match evaluate sc r with
    | Record r', _ ->
      snd @@ List.find (fun (k', _) -> String.equal k k') r',
      sc
    | _ ->
        assert false )
  | Extension (k, e, r) -> (
    match evaluate sc r with
    | Record r', _ ->
      Record ((k, fst @@ evaluate sc e) :: r'),
      sc
    | _ ->
        assert false )
  | Variant (v, e) ->
      Variant (v, fst @@ evaluate sc e)
    , sc
  | Var v ->
      Scope.find v sc, sc
  | Literal l ->
      Primitive (match l with
                | Int n -> Int n
                | String s -> String s), sc
  | Case (e, cs) ->
    let e', _ = evaluate sc e in
    (match e' with
    | Variant (v, variant_inner) ->
      let case_branch = List.find (function | (Ast.Expr.Tag_pat (lbl, _), _) -> String.equal v lbl
                                            | (Ast.Expr.Var_pat _, _) -> true) cs
      in
      (match case_branch with
      | (Ast.Expr.Tag_pat (_, var), body) ->
        fst @@ evaluate (Scope.add var variant_inner sc) body, sc
      | (Ast.Expr.Var_pat var, body) ->
        fst @@ evaluate (Scope.add var e' sc) body, sc
      )
    | _ -> assert false
    )
  | Sequence (e1, e2) ->
    evaluate (snd @@ evaluate sc e1) e2
