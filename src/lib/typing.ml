open Containers
module LookupTable = Map.Make (String)
module UVarTable = Map.Make (Int)

type context_var =
  | Context_var of Ast.var_name
  | Context_uvar of int

type context =
  { next_uvar : int (* TODO convert to context from complete and easy *)
  ; lt : Ast.type_ LookupTable.t
  ; uvar_assignments : Ast.type_ UVarTable.t
  }

let fresh_uvar ctx =
  (Ast.Type_uvar @@ ctx.next_uvar, { ctx with next_uvar = ctx.next_uvar + 1 })


(* TODO use result *)
let rec infer ctx annotated =
  let open Ast in
  match annotated.expr with
  | Var v ->
    ( match LookupTable.find_opt (Context_var v) ctx.lt with
    | None ->
        failwith @@ "Variable " ^ v ^ " not in context."
    | Some tp ->
        ({ annotated with tp }, ctx)
    | Record r ->
        let inferred_rcd, new_ctx =
          List.fold_map
            (fun ctx row_pair ->
              let label, e = row_pair in
              let inferred_e, new_ctx = infer ctx e in
              ((label, inferred_e), new_ctx))
            r
        in
        ( { expr = Record inferred_rcd
          ; tp = List.map (Pair.map2 (fun e -> e.tp) inferred_rcd)
          }
        , new_ctx )
    | Variant v ->
        let inferred_var, new_ctx =
          Pair.map2
          @@ List.fold_map
               (fun ctx row_pair ->
                 let label, e = row_pair in
                 let inferred_e, new_ctx = infer ctx e in
                 ((label, inferred_e), new_ctx))
               r
        in
        ( { expr = Variant inferred_var
          ; tp = List.map (Pair.map2 (fun e -> e.tp) @@ Pair.snd inferred_var)
          }
        , new_ctx )
    | Function f ->
        let rcd, e = f in
        let rcd_ctx =
          LookupTable.of_list @@ LookupTable.map (Pair.map1 Context_var) rcd
        in
        let uv, fun_ctx =
          fresh_uvar @@ LookupTable.union (fun _ _ b -> Some b) ctx.lt rcd_ctx
        in
        let checked_e, new_ctx = check fun_ctx e uv in
        (* revert to the old lookup table -- but preserve uvar
           assignments. We don't want to persist the bound variables
           from the function input, but we can't simply remove them
           from the lookup table. This should be OK, but might lose
           some changes made if implemented improperly. *)
        ( { expr = Function (rcd, checked_e); tp = Type_function (rcd, uv) }
        , { new_ctx with lt = ctx.lt } )
    | Application a ->
        let e1, e2 = a in
        let e1_tp, ctx_1 = infer ctx e1 in
        a )
