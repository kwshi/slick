open Containers

module Scope = Map.Make(String)

type var_name = string

type t =
  | Record of (string * t) list
  | Function of (t -> t)
  | Variant of string * (string * t) list

let rec pp ppf value =
  let open Fmt in
  ( match value with
    | Record r ->
      const pp_record r
    | Function _ ->
      any "<function>"
    | Variant (v, r) ->
      const string v ++ const pp_record r
  )
    ppf
    ()
and pp_record ppf =
  let open Format in
  (list ~sep:(return ",@ ") (pair ~sep:(return "=") string pp)
   |> within "{" "}"
  ) ppf


let rec evaluate (sc : t Scope.t) expr =
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

(* let rec evaluate ctx annotated =
 *   let open Ast.Expr in
 *   match annotated.expr with
 *   | Function (var, e) ->
 *       Value_function
 *         (fun val ->
 *           evaluate
 *             { lookup_table = LookupTable.add var val ctx.lookup_table }
 *             e)
 *   | Application (f, e) ->
 *     ( match evaluate ctx f with
 *     | Value_function fn -> f (evaluate ctx e)
 *     | _ ->
 *         failwith "not a function" )
 *   | Record r ->
 *       Value_record (evaluate_record ctx r)
 *   | Variant (s, r) ->
 *       Value_variant (s, evaluate_record ctx r)
 *   | Var s ->
 *       LookupTable.find s ctx.lookup_table *)
and evaluate_record ctx = List.map (Pair.map2 (evaluate ctx))
