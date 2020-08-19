open Containers
(* module LookupTable = Map.Make (String)
 * module UVarTable = Map.Make (Int) *)

type context_element =
  | Context_var of string * Ast.type_ (* Ast.var_name instead of string? *)
  | Context_evar of int
  | Context_tvar of int
  | Context_evar_assignment of int * Ast.type_
  | Context_marker of context_element (* this should only ever be an evar, so it probably could be an int *)

type context =
  { next_var : int (* this is for both evars and tvars - we could separate it though *)
  ; context  : context_element list
  }

(* Smart constructors for making fresh tvars/evars. Returns a tuple consisting of
  ( var as a type
  , var as a ctx element
  , new_ctx
  )
*)

let over_context f ctx = { ctx with context = f ctx.context }

let fresh_var to_type to_ctx_elem ctx =
  ( to_type ctx.next_var
  , to_ctx_elem ctx.next_var
  , {ctx with next_var = ctx.next_var + 1})

let fresh_tvar = fresh_var Ast.TVar Context_tvar

let fresh_evar = fresh_var Ast.EVar Context_evar

(* CONTEXT FUNCTIONS *)

(* solve_evar:
   takes in an evar ev (int), a type tp, and a context.

   Replaces the instance of Context_evar ev with Context_evar_assignment ev tp.

   Errors if there is no instance of ev in the context. Optionally errors if
   there are multiple instances (this should not happen).
*)

(* drop_ctx_from:
   takes in a context_element ce and a context.

   Removes everything in the context from ce forward, inclusive of ce (that is, ce is also removed).

   Errors if ce is not in the context.
*)

(* apply_ctx:
   takes in a context ctx and a type tp.

   if there is an evar in tp that has an assignment in ctx, the evar is replaced with its assignment.
   Similar to substitute, but for evars and potentially more than one variable.

   example:

   ctx = Context_evar 1, Context_evar_assignment 2 {}
   tp  = {x: evar 1} -> evar 2

   apply_ctx ctx tp = {x: evar 1 } -> {}
*)

(* insert_before_in_ctx
   takes in a context element ce, a list of context elements ces, and a context ctx.

   inserts ces before ce in the context.

   Errors if ce is not in the context.
*)

(* TYPE FUNCTIONS *)

(* occurs_check:
   takes in an evar ev and a type tp.

   traverses the type and asserts that ev is not present in tp.
   errors if it is.
*)

(* substitute:
   takes in an int tv representing a tvar, a type replace_tp to replace it with, and a type tp.

   traverses the type tp and replaces all instances of TVar tv with replace_tp.
*)

(*
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

   *)
