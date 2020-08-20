open Containers
module LookupTable = Map.Make (String)

type var_name = string

type value =
  | Value_record of (string * value) list
  | Value_function of (context -> value)
  | Value_variant of string * (string * value) list

and context = { lookup_table : value LookupTable.t }

let rec pp_value ppf value =
  let open CCFormat in
  ( match value with
  | Value_record r ->
      list ~sep:(return ",@ ") (pair ~sep:(return "=") string pp_value)
      |> within "{" "}"
      |> Fun.flip const r
  | Value_function _ ->
      return "<function>"
  | Value_variant _ ->
      return "var" )
    ppf
    ()


let rec evaluate ctx annotated =
  let open Ast.Expr in
  match annotated.expr with
  | Function (_, e) ->
      Value_function
        (fun fctx ->
          evaluate
            { lookup_table =
                LookupTable.union
                  (fun _ _ b -> Some b)
                  ctx.lookup_table
                  fctx.lookup_table
            }
            e)
  | Application (f, r) ->
    ( match evaluate ctx f with
    | Value_function fn ->
        { lookup_table = evaluate_record ctx r |> LookupTable.of_list } |> fn
    | _ ->
        failwith "not a function" )
  | Record r ->
      Value_record (evaluate_record ctx r)
  | Variant (s, r) ->
      Value_variant (s, evaluate_record ctx r)
  | Var s ->
      LookupTable.find s ctx.lookup_table


and evaluate_record ctx = List.map (Pair.map2 (evaluate ctx))
