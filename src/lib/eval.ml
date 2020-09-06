open Containers
open Fun

let rec evaluate (sc : Val.t Scope.t) expr =
  let apply f a = Val.Get.Exn.function_ f @@ evaluate sc a in
  let apply_to a f = apply f a in

  match expr.Ast.Expr.expr with
  | Assign (v, e, b) ->
      evaluate (Scope.add v (evaluate sc e) sc) b
  | Function (pat, e) ->
      Function
        (fun value ->
          match Ast.match_pat (pat, value) with
          | Some pat_bindings ->
              evaluate (Scope.add_list sc pat_bindings) e
          | None ->
              failwith "evaluate function: Pattern match failed.")
  | Application (f, e) ->
      evaluate sc f |> apply_to e
  | Record r ->
      Record (r |> List.map @@ Pair.map2 @@ evaluate sc)
  | Projection (r, lbl) ->
    ( match evaluate sc r with
    | Record r' ->
        snd @@ List.find (fun (lbl', _) -> String.(equal lbl lbl')) r'
    | _ ->
        assert false )
  | Extension (lbl, e, r) ->
    ( match evaluate sc r with
    | Record r' ->
        Record ((lbl, evaluate sc e) :: r')
    | _ ->
        assert false )
  | Variant (v, e) ->
      Variant (v, evaluate sc e)
  | Var v ->
      Scope.find v sc
  | Literal l ->
      Primitive (match l with Int n -> Int n | String s -> String s)
  | Case (e, cs) ->
      let e' = evaluate sc e in
      let sc', case_inner =
        List.find_map
          (fun (pat, case_inner) ->
            match Ast.match_pat (pat, e') with
            | Some pat_bindings ->
                Some (Scope.add_list sc pat_bindings, case_inner)
            | None ->
                None)
          cs
        |> Option.get_exn
      in
      evaluate sc' case_inner
  | Bop (o, a, b) ->
      Scope.find o sc |> apply_to a |> apply_to b
  | Uop (o, a) ->
      Scope.find o sc |> apply_to a
