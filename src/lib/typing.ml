open Containers
open Fun
(* module LookupTable = Map.Make (String)
 * module UVarTable = Map.Make (Int) *)

type context_element =
  | Context_var of string * Ast.Type.t (* Ast.var_name instead of string? *)
  | Context_evar of int
  | Context_tvar of int
  | Context_evar_assignment of int * Ast.Type.t
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

let fresh_tvar = fresh_var (fun i -> Ast.Type.TVar i) (fun i -> Context_tvar i)

let fresh_evar = fresh_var (fun i -> Ast.Type.EVar i) (fun i -> Context_evar i)

(* CONTEXT FUNCTIONS *)

(* solve_evar:
   takes in an evar ev (int), a type tp, and a context.

   Replaces the instance of Context_evar ev with Context_evar_assignment ev tp.

   Errors if there is no instance of ev in the context. Optionally errors if
   there are multiple instances (this should not happen).
*)
let solve_evar ev tp =
  over_context @@ fun ctx ->
     let found, ctx' =
       List.fold_map
         (fun found ->
            function
            | Context_evar ev' when Int.(ev = ev') ->
              if found then failwith "solve_evar multiple matches"
              else true, Context_evar_assignment (ev, tp)
            | ce ->
              found, ce
         )
         false
         ctx
     in
     if found then ctx' else failwith "solve_evar not found"

(* drop_ctx_from:
   takes in a context_element ce and a context.

   Removes everything in the context from ce forward, inclusive of ce (that is, ce is also removed).

   Errors if ce is not in the context.
*)
let drop_ctx_from ce =
  over_context @@ fun ctx ->
  let found, ctx' = List.fold_filter_map 
    (fun found ce' ->
      (match found, Stdlib.(ce' = ce) (* TODO get rid of polymorphic comparison *) with
       | true, true -> failwith "drop_ctx_from multiple matches"
       | true, false | false, true -> true, None
       | false, false -> false, Some ce'
      )
    )
    false
    ctx
  in
  if found then ctx' else failwith "drop_ctx_from not found"
    

(* apply_ctx:
   takes in a context ctx and a type tp.

   if there is an evar in tp that has an assignment in ctx, the evar is replaced with its assignment.
   Similar to substitute, but for evars and potentially more than one variable.

   example:

   ctx = Context_evar 1, Context_evar_assignment 2 {}
   tp  = {x: evar 1} -> evar 2

   apply_ctx ctx tp = {x: evar 1 } -> {}
*)
let apply_ctx ctx =
  let rec go =
    function (* TODO don't use ast types *)
    | Ast.Type.Record r -> Ast.Type.Record (record r)
    | Ast.Type.Variant () -> Ast.Type.Variant ()
    | Ast.Type.Function (r, t) -> Ast.Type.Function (record r, go t)
    | Ast.Type.EVar ev ->
      ctx.context
      |> List.find_map (function
          | Context_evar_assignment (ev', t) when Int.(ev = ev') -> Some t
          | _ -> None
        )
      |> Option.get_or ~default:(Ast.Type.EVar ev)

    | Ast.Type.TVar tv -> Ast.Type.TVar tv
  and record r = List.map (Pair.map2 go) r in
  go
    

(* insert_before_in_ctx
   takes in a context element ce, a list of context elements ces, and a context ctx.

   inserts ces before ce in the context.

   Errors if ce is not in the context.
*)
let insert_before_in_ctx ce ces =
  (* TODO (maybe) use tail-recursive fold_left followed by List.rev to save
     memory, but that's sort of an unnecessary optimization *)
  over_context @@ List.fold_right
    (fun ce' acc ->
       (if Stdlib.(ce' = ce) (* TODO get rid of polymorphic comparison *)
        then ces
        else []
       ) @ ce :: acc
    )
    []
  
    
(* TYPE FUNCTIONS *)

(* occurs_check:
   takes in an evar ev and a type tp.

   traverses the type and asserts that ev is not present in tp.
   errors if it is.
*)
let occurs_check ev =
  let rec go =
    function
    | Ast.Type.Record r -> record r
    | Ast.Type.Variant () -> ()
    | Ast.Type.Function (r, t) -> record r; go t
    | Ast.Type.EVar ev' ->
      if Int.(ev = ev')
      then failwith "occurs_check"
    | Ast.Type.TVar _ -> ()
  and record r = List.iter (snd %> go) r in
  go

(* substitute:
   takes in an int tv representing a tvar, a type replace_tp to replace it with, and a type tp.

   traverses the type tp and replaces all instances of TVar tv with replace_tp.
*)
let substitute tv ~replace_with =
  let rec go =
    function
    | Ast.Type.Record r -> Ast.Type.Record (record r)
    | Ast.Type.Variant () -> Ast.Type.Variant ()
    | Ast.Type.Function (r, t) -> Ast.Type.Function (record r, go t)
    | Ast.Type.EVar ev -> Ast.Type.EVar ev
    | Ast.Type.TVar tv' ->
      if Int.(tv = tv') then replace_with else Ast.Type.TVar tv'
  and record r = List.map (Pair.map2 go) r in
  go
  


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
