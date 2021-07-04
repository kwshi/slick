open Containers
open Fun

let rec evaluate (sc : Val.t Scope.t) expr =
  let apply f a = Val.Get.Exn.function_ f @@ evaluate sc a in
  let apply_to a f = apply f a in

  match expr.Slick_ast.Expr.expr with
  | Assign (v, e, b) ->
      evaluate (Scope.add v (evaluate sc e) sc) b
  | Function (pat, e) ->
      Function
        (fun value ->
          match Val.match_pat (pat, value) with
          | Some pat_bindings ->
              evaluate (Scope.add_list sc pat_bindings) e
          | None ->
              failwith "evaluate function: Pattern match failed." )
  | Application (f, e) ->
      evaluate sc f |> apply_to e
  | Tuple t ->
      Tuple
        { unlabeled = List.map (evaluate sc) t.unlabeled
        ; labeled = List.map (Pair.map_snd @@ evaluate sc) t.labeled
        }
  | Projection (r, lbl) ->
    ( match evaluate sc r with
    | Tuple r' ->
      ( match int_of_string_opt lbl with
      | Some n ->
          List.nth r'.unlabeled n
      | None ->
          snd @@ List.find (fun (lbl', _) -> String.(equal lbl lbl')) r'.labeled
      )
    | _ ->
        assert false )
  | Extension (lbl, e, r) ->
    ( match evaluate sc r with
    | Tuple r' ->
        Tuple
          { labeled = (lbl, evaluate sc e) :: r'.labeled
          ; unlabeled = r'.unlabeled
          }
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
            match Val.match_pat (pat, e') with
            | Some pat_bindings ->
                Some (Scope.add_list sc pat_bindings, case_inner)
            | None ->
                None )
          cs
        |> Option.get_exn_or "case"
      in
      evaluate sc' case_inner
  | Bop (o, a, b) ->
      Scope.find o sc |> apply_to a |> apply_to b
  | Uop (o, a) ->
      Scope.find o sc |> apply_to a
