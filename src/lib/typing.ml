open Containers
open Fun

(* module LookupTable = Map.Make (String)
 * module UVarTable = Map.Make (Int) *)

type var_name = string

let pp_var_name = Format.string

type label = string

let pp_label = Format.string

type tail =
  | Tail_evar of int
  | Tail_tvar of var_name
  [@@deriving show]
type t =
  | Record of row
  | Variant of unit
  | Function of (t * t)
  | EVar of int
  | TVar of var_name
  | Forall of var_name * t
  [@@deriving show]
and row = (label * t) list * tail option
  [@@deriving show]

type context_element =
  | Context_var of string * t (* Ast.var_name instead of string? *)
  | Context_row_evar of int
  | Context_row_tvar of string
  | Context_evar of int
  | Context_tvar of string
  | Context_evar_assignment of int * t
  | Context_row_evar_assignment of int * row
  | Context_marker of context_element

let map_row f = Pair.map1 (List.map (Pair.map2 f))
let iter_row f = fst %> List.iter (snd %> f)


(* this should only ever be an evar, so it probably could be an int *)

type context =
  { next_var : int
        (* this is for both evars and tvars - we could separate it though *)
  ; context : context_element list
  }

let empty_ctx = { next_var = 0; context = [] }

(* Smart constructors for making fresh evars. Returns a tuple consisting of
  ( var as a type
  , var as a ctx element
  , new_ctx
  )
*)

let fresh_evar ctx =
  ( EVar ctx.next_var
  , Context_evar ctx.next_var
  , ctx.next_var
  , { ctx with next_var = ctx.next_var + 1 } )

let fresh_row_evar ctx =
  ( Tail_evar ctx.next_var
  , Context_row_evar ctx.next_var
  , ctx.next_var
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

let solve_row_evar ev r =
  over_context
  @@ fun ctx ->
  let found, ctx' =
    List.fold_map
      (fun found -> function Context_row_evar ev' when Int.(ev = ev') ->
          if found
          then failwith "solve_row_evar multiple matches"
          else (true, Context_row_evar_assignment (ev, r)) | ce -> (found, ce))
      false
      ctx
  in
  if found then ctx' else failwith "solve_row_evar not found"



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
    | Record r ->
        Record (row r)
    | Function (t1, t2) ->
        Function (go t1, go t2)
    | EVar ev ->
        ctx.context
        |> List.find_map (function
            | Context_evar_assignment (ev', t) when Int.(ev = ev') ->
              Some t
            | _ ->
              None)
        |> Option.get_or ~default:(EVar ev)
    | Forall (tv, tp) ->
        Forall (tv, go tp)
    | t ->
      t 
  and row (l, t) =
    let l' = List.map (Pair.map2 go) l in
    match t with
    | Some (Tail_evar ev) ->
      ctx.context
      |> List.find_map (function
          | Context_row_evar_assignment (ev', r) when Int.(ev = ev') ->
            Some r
          | _ ->
            None
        )
      |> Option.map_or ~default:(l', t) (fun (l'', t') -> (l' @ l'', t'))
        
    | _ ->
      (l', t)
  in
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
  let check_tail =
    function
    | Some (Tail_evar ev') when Int.(ev = ev') ->
      failwith "occurs_check"
    | _ ->
      ()
  in
  let rec go = function
    | Function (t1, t2) ->
      go t1;
      go t2;
    | EVar ev' when Int.(ev = ev') ->
      failwith "occurs_check"
    | Record r ->
      check_tail (snd r);
      fst r |> List.iter (snd %> go)
    | Forall (_, tp) ->
      go tp
    | _ ->
      ()
  in
  go


(* substitute:
   takes in an int tv representing a tvar, a type replace_tp to replace it with, and a type tp.

   traverses the type tp and replaces all instances of TVar tv with replace_tp.
*)
let substitute tv ~replace_with =
  let rec go = function
    | Record r ->
        Record (Pair.map1 (List.map @@ Pair.map2 go) r)
    | Variant () ->
        Variant ()
    | Function (t1, t2) ->
        Function (go t1, go t2)
    | EVar ev ->
        EVar ev
    | TVar tv' ->
        if String.(equal tv tv') then replace_with else TVar tv'
    | Forall (tv', tp) ->
        if String.(equal tv tv')
        then Forall (tv', tp)
        else Forall (tv', go tp)
  in
  go


(* INFERENCE and CHECKING *)

let rec infer_top ctx annotated =
  let inferred, new_ctx = infer ctx annotated in
  ({inferred with tp=apply_ctx new_ctx inferred.tp}, new_ctx)

and infer ctx (annotated : Ast.Expr.Untyped.t) : t Ast.Expr.t * context=
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
            Record (List.map (Pair.map2 (fun e -> e.Ast.Expr.tp)) inferred_rcd, None)
        }
      , new_ctx )
  (* ->I => *)
  | Ast.Expr.Function (var, e) ->
      let arg_ev_tp, arg_ev_ce, _, ctx' = fresh_evar ctx in
      let ret_ev_tp, ret_ev_ce, _, ctx'' = fresh_evar ctx' in
      (* The context marker is added to make it possible to drop from the context even if no vars are added
         I just use ev_ce since it's available. *)
      let fun_ctx =
        append_ctx [ arg_ev_ce; ret_ev_ce; Context_var (var, arg_ev_tp)] ctx''
      in
      let e_checked, new_ctx = check fun_ctx e ret_ev_tp in
      let fun_tp = Function (arg_ev_tp, ret_ev_tp) in
      ( { Ast.Expr.expr = Function (var, e_checked); Ast.Expr.tp = fun_tp }
      , drop_ctx_from (Context_var (var,arg_ev_tp)) new_ctx )
  (* ->E *)
  | Ast.Expr.Application (e1, e2) ->
      let e1_inferred, new_ctx = infer ctx e1 in
      let e2_inferred, output_tp, new_ctx' =
        infer_app new_ctx (apply_ctx new_ctx e1_inferred.tp) e2
      in
      ({ expr = Application (e1_inferred, e2_inferred); tp = output_tp }, new_ctx')
  (* TODO *)
  | Ast.Expr.Variant _ ->
      ({ expr = Ast.Expr.Variant ("", []); tp = Variant () }, ctx)


and check ctx annotated tp =
  let open Ast in
  match (annotated.expr, tp) with
  (* forall I *)
  | _, Forall (tv, tp') ->
      let ctx' = append_ctx [ Context_tvar tv ] ctx in
      let checked_e, new_ctx = check ctx' annotated tp' in
      ( { checked_e with tp = Forall (tv, checked_e.tp) }
      , drop_ctx_from (Context_tvar tv) new_ctx )
  (* -> I *)
  | Expr.Function (var, e), Function (arg_tp, return_tp) ->
      let fun_ctx = append_ctx [ Context_var (var,arg_tp) ] ctx in
      let checked_e, new_ctx = check fun_ctx e return_tp in
      ( { expr = Function (var, checked_e); tp = Function (arg_tp, return_tp) }
      , drop_ctx_from ( Context_var (var, arg_tp)) new_ctx )
  (* Sub *)
  | _ ->
      let e_inferred, ctx' = infer ctx annotated in
      let new_ctx =
        subsumes ctx' (apply_ctx ctx' e_inferred.tp) (apply_ctx ctx' tp)
      in
      ({ e_inferred with tp }, new_ctx)


and infer_app ctx tp e1 =
  match tp with
  (* Forall App *)
  | Forall (tv, forall_inner) ->
    let ev_tp, ev_ce, _, ctx' = fresh_evar ctx in
    let subst_ctx = append_ctx [ev_ce] ctx' in
    let subst_forall_inner = substitute tv ~replace_with:(ev_tp) forall_inner in
    infer_app subst_ctx subst_forall_inner e1
  (* -> App *)
  | Function (arg_tp, return_tp) ->
    let checked_e1, new_ctx = check ctx e1 arg_tp in
    (checked_e1, return_tp, new_ctx)
  (* EVar App *)
  | EVar ev ->
    let arg_ev_tp, arg_ev_ce, _, ctx'  = fresh_evar ctx in
    let ret_ev_tp, ret_ev_ce, _, ctx'' = fresh_evar ctx' in
    (* The EVar should be unsovled if we find it, so it's safe to use 'Context_evar ev' *)
    let inserted_ctx = insert_before_in_ctx (Context_evar ev) [arg_ev_ce; ret_ev_ce] ctx'' in
    let solved_ctx   = solve_evar ev (Function (arg_ev_tp, ret_ev_tp)) inserted_ctx in
    let checked_e1, new_ctx = check solved_ctx e1 arg_ev_tp in
    (checked_e1, ret_ev_tp, new_ctx)
  | _ -> failwith "infer_app: Got unexpected type."

and subsumes ctx tp1 tp2 =
  match (tp1, tp2) with
  (* EVar *)
  | EVar ev1, EVar ev2 when Int.(ev1 = ev2) -> ctx
  (* TVar *)
  | TVar tv1, TVar tv2 when String.(equal tv1 tv2) -> ctx
  (* Forall L *)
  | Forall (tv, forall_inner), tp2 ->
    let ev_tp, ev_ce, _, ctx' = fresh_evar ctx in
    let marker = Context_marker ev_ce in
    let subst_ctx = append_ctx [marker; ev_ce] ctx' in
    let subst_forall_inner = substitute tv ~replace_with:(ev_tp) forall_inner in
    subsumes subst_ctx subst_forall_inner tp2
    |> drop_ctx_from marker
  (* Forall R *)
  | tp1, Forall (tv, forall_inner) ->
    let ctx' = append_ctx [Context_tvar tv] ctx in
    subsumes ctx' tp1 forall_inner
    |> drop_ctx_from (Context_tvar tv)
  (* -> *)
  | Function (arg_tp1, return_tp1), Function(arg_tp2, return_tp2) ->
    let ctx' = subsumes ctx arg_tp2 arg_tp1 in
    subsumes ctx' (apply_ctx ctx' return_tp1) (apply_ctx ctx' return_tp2)
  (* Record *)
  | Record r1, Record r2 ->
    ignore r1; ignore r2;
    failwith "subsumes: record unimplemented"
  (* InstantiateL *)
  | EVar ev1, tp2 -> instantiateL ctx ev1 tp2
  (* InstantiateR *)
  | tp1, EVar ev2 -> instantiateR ctx tp1 ev2
  | _ -> failwith "subsumes: unimplemented types"

and instantiateL ctx ev tp =
  match tp with
  (* InstLArr *)
  | Function (arg_tp, ret_tp) ->
    let arg_ev_tp, arg_ev_ce, arg_ev, ctx'  = fresh_evar ctx in
    let ret_ev_tp, ret_ev_ce, ret_ev, ctx'' = fresh_evar ctx' in
    (* The EVar should be unsovled if we find it, so it's safe to use 'Context_evar ev' *)
    let inserted_ctx = insert_before_in_ctx (Context_evar ev) [arg_ev_ce; ret_ev_ce] ctx'' in
    let solved_ctx   = solve_evar ev (Function (arg_ev_tp, ret_ev_tp)) inserted_ctx in
    let new_ctx = instantiateR solved_ctx arg_tp arg_ev in
    instantiateL new_ctx ret_ev (apply_ctx new_ctx ret_tp)
  (* InstLReach *)
  | EVar ev2 -> instantiateReach ctx ev ev2
  (* InstLRcd *)
  | Record r -> ignore r; failwith "instantiateL: record unimplemented"
  (* InstLAllR *)
  | Forall (tv, forall_inner) ->
    let ctx' = append_ctx [Context_tvar tv] ctx in
    instantiateL ctx' ev forall_inner
    |> drop_ctx_from (Context_tvar tv)
  | _ -> failwith "instantiateL: unimplemented"

and instantiateR ctx tp ev =
  match tp with
  (* InstLArr *)
  | Function (arg_tp, ret_tp) ->
    let arg_ev_tp, arg_ev_ce, arg_ev, ctx'  = fresh_evar ctx in
    let ret_ev_tp, ret_ev_ce, ret_ev, ctx'' = fresh_evar ctx' in
    (* The EVar should be unsovled if we find it, so it's safe to use 'Context_evar ev' *)
    let inserted_ctx = insert_before_in_ctx (Context_evar ev) [arg_ev_ce; ret_ev_ce] ctx'' in
    let solved_ctx   = solve_evar ev (Function (arg_ev_tp, ret_ev_tp)) inserted_ctx in
    let new_ctx = instantiateL solved_ctx arg_ev arg_tp in
    instantiateR new_ctx (apply_ctx new_ctx ret_tp) ret_ev
  (* InstLReach *)
  | EVar ev2 -> instantiateReach ctx ev ev2
  (* InstLRcd *)
  | Record r -> ignore r; failwith "instantiateR: record unimplemented"
  (* InstLAllR *)
  | Forall (tv, forall_inner) ->
    let forall_ev_tp, forall_ev_ce, _, ctx' = fresh_evar ctx in
    let ctx'' = append_ctx [Context_marker forall_ev_ce; forall_ev_ce] ctx' in
    instantiateR ctx'' (substitute tv ~replace_with:forall_ev_tp forall_inner) ev
    |> drop_ctx_from (Context_marker forall_ev_ce)
  | _ -> failwith "instantiateR: unimplemented"

(* find where ev1 and ev2 are located in the context. Assign the later one to the earlier one.*)
and instantiateReach ctx ev1 ev2 =
  let find_ev ev = List.find_idx
     (function Context_evar ev' when Int.(ev=ev') -> true
      | _ -> false)
     ctx.context in
  let ev1_index = find_ev ev1 in
  let ev2_index = find_ev ev2 in
  match (ev1_index, ev2_index) with
  | Some (ev1_i, _), Some (ev2_i, _)->
    if ev1_i <= ev2_i
    then solve_evar ev2 (EVar ev1) ctx
    else solve_evar ev1 (EVar ev2) ctx
  | _ ->
    failwith "instantiateReach: evar not in context."
