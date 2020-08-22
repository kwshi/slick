open Containers
open Fun

(* module LookupTable = Map.Make (String)
 * module UVarTable = Map.Make (Int) *)

type context_element =
  | Context_var of string * Ast.Type.t (* Ast.var_name instead of string? *)
  | Context_evar of int
  | Context_tvar of string
  | Context_evar_assignment of int * Ast.Type.t
  | Context_marker of context_element

(* this should only ever be an evar, so it probably could be an int *)

type context =
  { next_var : int
        (* this is for both evars and tvars - we could separate it though *)
  ; context : context_element list
  }

(* Smart constructors for making fresh evars. Returns a tuple consisting of
  ( var as a type
  , var as a ctx element
  , new_ctx
  )
*)

let fresh_evar ctx =
  ( Ast.Type.EVar ctx.next_var
  , Context_evar ctx.next_var
  , { ctx with next_var = ctx.next_var + 1 } )


let over_context f ctx = { ctx with context = f ctx.context }

(* CONTEXT FUNCTIONS *)

(* solve_evar:
   takes in an evar ev (int), a type tp, and a context.

   Replaces the instance of Context_evar ev with Context_evar_assignment ev tp.

   Errors if there is no instance of ev in the context. Optionally errors if
   there are multiple instances (this should not happen).
*)
let solve_evar ev tp =
  over_context
  @@ fun ctx ->
  let found, ctx' =
    List.fold_map
      (fun found -> function Context_evar ev' when Int.(ev = ev') ->
            if found
            then failwith "solve_evar multiple matches"
            else (true, Context_evar_assignment (ev, tp)) | ce -> (found, ce))
      false
      ctx
  in
  if found then ctx' else failwith "solve_evar not found"


let lookup_var v ctx =
  List.find_map
    (function
      | Context_var (v', tp) when String.(equal v v') -> Some tp | _ -> None)
    ctx.context
  |> Option.get_lazy (fun () ->
         failwith @@ "lookup_var unbound var: " ^ v ^ ".")


(* drop_ctx_from:
   takes in a context_element ce and a context.

   Removes everything in the context from ce forward, inclusive of ce (that is, ce is also removed).

   Errors if ce is not in the context.
*)
let drop_ctx_from ce =
  over_context
  @@ fun ctx ->
  let found, ctx' =
    List.fold_filter_map
      (fun found ce' ->
        match
          (found, Stdlib.(ce' = ce))
          (* TODO get rid of polymorphic comparison *)
        with
        | true, true ->
            failwith "drop_ctx_from multiple matches"
        | true, false | false, true ->
            (true, None)
        | false, false ->
            (false, Some ce'))
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
  let rec go = function
    (* TODO don't use ast types *)
    | Ast.Type.Record r ->
        Ast.Type.Record (record r)
    | Ast.Type.Variant () ->
        Ast.Type.Variant ()
    | Ast.Type.Function (r, t) ->
        Ast.Type.Function (record r, go t)
    | Ast.Type.EVar ev ->
        ctx.context
        |> List.find_map (function
               | Context_evar_assignment (ev', t) when Int.(ev = ev') ->
                   Some t
               | _ ->
                   None)
        |> Option.get_or ~default:(Ast.Type.EVar ev)
    | Ast.Type.TVar tv ->
        Ast.Type.TVar tv
    | Ast.Type.Forall (tv, tp) ->
        Ast.Type.Forall (tv, go tp)
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
  over_context
  @@ List.fold_right
       (fun ce' acc ->
         ( if Stdlib.(ce' = ce) (* TODO get rid of polymorphic comparison *)
         then ces
         else [] )
         @ (ce :: acc))
       []


let append_ctx ces = over_context @@ fun context -> List.append context ces

(* TYPE FUNCTIONS *)

(* occurs_check:
   takes in an evar ev and a type tp.

   traverses the type and asserts that ev is not present in tp.
   errors if it is.
*)
let occurs_check ev =
  let rec go = function
    | Ast.Type.Record r ->
        record r
    | Ast.Type.Variant () ->
        ()
    | Ast.Type.Function (r, t) ->
        record r ;
        go t
    | Ast.Type.EVar ev' ->
        if Int.(ev = ev') then failwith "occurs_check"
    | Ast.Type.TVar _ ->
        ()
    | Ast.Type.Forall (_, tp) ->
        go tp
  and record r = List.iter (snd %> go) r in
  go


(* substitute:
   takes in an int tv representing a tvar, a type replace_tp to replace it with, and a type tp.

   traverses the type tp and replaces all instances of TVar tv with replace_tp.
*)
let substitute tv ~replace_with =
  let rec go = function
    | Ast.Type.Record r ->
        Ast.Type.Record (record r)
    | Ast.Type.Variant () ->
        Ast.Type.Variant ()
    | Ast.Type.Function (r, t) ->
        Ast.Type.Function (record r, go t)
    | Ast.Type.EVar ev ->
        Ast.Type.EVar ev
    | Ast.Type.TVar tv' ->
        if String.(equal tv tv') then replace_with else Ast.Type.TVar tv'
    | Ast.Type.Forall (tv', tp) ->
        if String.(equal tv tv')
        then Ast.Type.Forall (tv', tp)
        else Ast.Type.Forall (tv', go tp)
  and record r = List.map (Pair.map2 go) r in
  go


(* INFERENCE and CHECKING *)

let rec infer ctx (annotated : Ast.Expr.Untyped.t) : Ast.Type.t Ast.Expr.t * context=
  match annotated.Ast.Expr.expr with
  (* Var  *)
  | Ast.Expr.Var v ->
      let tp = lookup_var v ctx in
      ({ Ast.Expr.expr = Var v; tp }, ctx)
  (* RcdI => *)
  | Ast.Expr.Record r ->
      let new_ctx, inferred_rcd =
        List.fold_map
          (fun ctx (label, e) ->
            let inferred_e, new_ctx = infer ctx e in
            (new_ctx, (label, inferred_e)))
          ctx
          r
      in
      ( { expr = Record inferred_rcd
        ; tp =
            Record (List.map (Pair.map2 (fun e -> e.Ast.Expr.tp)) inferred_rcd)
        }
      , new_ctx )
  (* ->I => *)
  | Ast.Expr.Function (r, e) ->
      let ev_tp, ev_ce, ctx' = fresh_evar ctx in
      (* The context marker is added to make it possible to drop from the context even if no vars are added
         I just use ev_ce since it's available. *)
      let marker = Context_marker ev_ce in
      let fun_ctx =
        append_ctx
          ( ev_ce
          :: marker
          :: List.map (fun (lbl, tp) -> Context_var (lbl, tp)) r )
          ctx'
      in
      let e_checked, new_ctx = check fun_ctx e ev_tp in
      let fun_tp = Ast.Type.Function (r, ev_tp) in
      ( { Ast.Expr.expr = Function (r, e_checked); Ast.Expr.tp = fun_tp }
      , drop_ctx_from marker new_ctx )
  (* ->E *)
  | Ast.Expr.Application (e, r) ->
      let e_inferred, new_ctx = infer ctx e in
      let r_inferred, output_tp, new_ctx' =
        infer_app new_ctx (apply_ctx new_ctx e_inferred.tp) r
      in
      ({ expr = Application (e_inferred, r_inferred); tp = output_tp }, new_ctx')
  (* TODO *)
  | Ast.Expr.Variant _ ->
      ({ expr = Ast.Expr.Variant ("", []); tp = Ast.Type.Variant () }, ctx)


and check ctx annotated tp =
  let open Ast in
  match (annotated.expr, tp) with
  (* forall I *)
  | _, Type.Forall (tv, tp') ->
      let ctx' = append_ctx [ Context_tvar tv ] ctx in
      let checked_e, new_ctx = check ctx' annotated tp' in
      ( { checked_e with tp = Type.Forall (tv, checked_e.tp) }
      , drop_ctx_from (Context_tvar tv) new_ctx )
  (* -> I *)
  | Function (r1, e), Type.Function (r2, return_tp) ->
      (* Hack: we create an evar we don't use so we can make a context marker out of it.
         Maybe we should just support creating fresh context markers. *)
      let _, ev_ce, ctx' =
        fresh_evar @@ subsumes ctx (Type.Record r1) (Type.Record r2)
      in
      let marker = Context_marker ev_ce in
      let fun_ctx =
        append_ctx
          ( Context_marker ev_ce
          :: List.map (fun (l, t) -> Context_var (l, t)) r2 )
          ctx'
      in
      let checked_e, new_ctx = check fun_ctx e return_tp in
      (* Should we stick with the type we're checking against or the declared type for the record input?
         Right now we do the declared type. *)
      ( { expr = Function (r1, checked_e); tp = Type.Function (r1, return_tp) }
      , drop_ctx_from marker new_ctx )
  (* Sub *)
  | _ ->
      let e_inferred, ctx' = infer ctx annotated in
      let new_ctx =
        subsumes ctx' (apply_ctx ctx' e_inferred.tp) (apply_ctx ctx' tp)
      in
      ({ e_inferred with tp }, new_ctx)


and infer_app ctx tp args =
  let open Ast in
  match tp with
  (* Forall App *)
  | Type.Forall (tv, forall_inner) ->
    let ev_tp, ev_ce, ctx' = fresh_evar ctx in
    let subst_ctx = append_ctx [ev_ce] ctx' in
    let subst_forall_inner = substitute tv ~replace_with:(ev_tp) forall_inner in
    infer_app subst_ctx subst_forall_inner args
  (* -> App *)
  | Type.Function (arg_types, return_tp) ->
    let checked_rcd, new_ctx = check ctx (Expr.Untyped.make_record args) (Type.Record arg_types) in
    (match checked_rcd.expr with
     | Expr.Record checked_args -> (checked_args, return_tp, new_ctx)
     | _ -> failwith "infer_app: got non-record (this shouldn't happen)"
    )
  (* EVar App *)
  | Type.EVar ev -> ignore ev; failwith "infer_app: EVar app unimplemented"
  | _ -> failwith "infer_app: Got unexpected type."

and subsumes ctx tp1 tp2 =
  let open Ast in
  match (tp1, tp2) with
  (* EVar *)
  | Type.EVar ev1, Type.EVar ev2 when Int.(ev1 = ev2) -> ctx
  (* TVar *)
  | Type.TVar tv1, Type.TVar tv2 when String.(equal tv1 tv2) -> ctx
  (* Forall L *)
  | Type.Forall (tv, forall_inner), tp2 ->
    let ev_tp, ev_ce, ctx' = fresh_evar ctx in
    let marker = Context_marker ev_ce in
    let subst_ctx = append_ctx [marker; ev_ce] ctx' in
    let subst_forall_inner = substitute tv ~replace_with:(ev_tp) forall_inner in
    subsumes subst_ctx subst_forall_inner tp2
    |> drop_ctx_from marker
  (* Forall R *)
  | tp1, Type.Forall (tv, forall_inner) ->
    let ctx' = append_ctx [Context_tvar tv] ctx in
    subsumes ctx' tp1 forall_inner
    |> drop_ctx_from (Context_tvar tv)
  (* -> *)
  | Type.Function (r1, return_tp1), Type.Function(r2, return_tp2) ->
    let ctx' = subsumes ctx (Type.Record r2) (Type.Record r1) in
    subsumes ctx' (apply_ctx ctx' return_tp1) (apply_ctx ctx' return_tp2)
  (* Record *)
  | Type.Record r1, Type.Record r2 ->
    ignore r1; ignore r2;
    failwith "subsumes: record unimplemented"
  (* InstantiateL *)
  | Type.EVar ev1, tp2 -> instantiateL ctx ev1 tp2
  (* InstantiateR *)
  | tp1, Type.EVar ev2 -> instantiateR ctx tp1 ev2
  | _ -> failwith "subsumes: unimplemented types"

and instantiateL ctx ev tp =
  let open Ast in
  match tp with
  (* InstLArr *)
  | Type.Function (r, return_tp) -> ignore (r, return_tp); failwith "instantiateL: function unimplemented"
  (* InstLReach *)
  | Type.EVar ev2 -> instantiateReach ctx ev ev2
  (* InstLRcd *)
  | Type.Record r -> ignore r; failwith "instantiateL: record unimplemented"
  (* InstLAllR *)
  | Type.Forall (tv, forall_inner) ->
    let ctx' = append_ctx [Context_tvar tv] ctx in
    instantiateL ctx' ev forall_inner
    |> drop_ctx_from (Context_tvar tv)
  | _ -> failwith "dead"

and instantiateR ctx tp ev = ignore (ctx, tp, ev); failwith "instantiateR: unimplemented"

(* find where ev1 and ev2 are located in the context. Assign the later one to the earlier one.*)
and instantiateReach ctx ev1 ev2 = ignore (ctx, ev1, ev2); failwith "instantiateReach: unimplemented"
