open Containers
open Fun

let rec evaluate (sc : Val.t Scope.t) expr =

  let apply f a = Val.Get.Exn.function_ f @@ evaluate sc a in
  let apply_to a f = apply f a in

  match expr.Ast.Expr.expr with
  | Assign (v, e, b) ->
      evaluate (Scope.add v (evaluate sc e) sc) b
  | Function (v, e) ->
      Function (fun value -> evaluate (Scope.add v value sc) e)
  | Application (f, e) -> 
    evaluate sc f |> apply_to e
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
    Primitive
      (match l with
       | Int n -> Int n
       | String s -> String s
      )
  | Case (e, cs) ->
    (match evaluate sc e with
     | Variant (tag, e) ->
       let sc', e' =
         List.find_map
           (function
             | (Ast.Expr.Tag_pat (tag', var), e') when String.equal tag tag' ->
               Some (Scope.add var e sc, e')
                 
             | (Var_pat var, e') ->
               Some (Scope.add var (Val.Variant (tag, e)) sc, e')
                 
             | _ ->
               None
                 
           )
           cs
         |> Option.get_exn
       in
       evaluate sc' e'

     | _ -> assert false
    )
  | Bop (o, a, b) ->
    Scope.find o sc
    |> apply_to a
    |> apply_to b
  | Uop (o, a) ->
    Scope.find o sc
    |> apply_to a

  
    
