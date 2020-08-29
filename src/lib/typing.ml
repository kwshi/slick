open Containers
open Fun
module RowMap = Map.Make (String)

type context_element =
  | Context_var of string * Type.t (* Ast.var_name instead of string? *)
  | Context_row_evar of int
  | Context_row_tvar of string
  | Context_evar of int
  | Context_tvar of string
  | Context_evar_assignment of int * Type.t
  | Context_row_evar_assignment of int * Type.row
  | Context_marker of context_element
[@@deriving show]

(* Row functions *)

let row_map_difference m1 m2 =
  RowMap.filter (fun l _ -> RowMap.find_opt l m2 |> Option.is_none) m1


let row_map_zip =
  let f _ a_opt b_opt =
    match (a_opt, b_opt) with Some a, Some b -> Some (a, b) | _ -> None
  in
  RowMap.merge f


let map_row f = Pair.map1 (List.map (Pair.map2 f))

let iter_row f = fst %> List.iter (snd %> f)

(* this should only ever be an evar, so it probably could be an int *)

type context =
  { next_var : int
        (* this is for both evars and tvars - we could separate it though *)
  ; context : context_element list
  }

let empty_ctx = { next_var = 0; context = [] }

(* Debug functions *)

let print_ctx (ctx : context) =
  print_string
  @@ List.to_string
       ?start:(Some "[")
       ?stop:(Some "]")
       show_context_element
       ctx.context ;
  print_newline ()


let print_tp tp =
  Type.pp Format.stdout tp ;
  Format.print_newline ()


(* Smart constructors for making fresh evars. Returns a tuple consisting of
  ( var as a type
  , var as a ctx element
  , new_ctx
  )
*)

let fresh_evar ctx =
  ( Type.EVar ctx.next_var
  , Context_evar ctx.next_var
  , ctx.next_var
  , { ctx with next_var = ctx.next_var + 1 } )


let fresh_row_evar ctx =
  ( Type.Tail_evar ctx.next_var
  , Context_row_evar ctx.next_var
  , ctx.next_var
  , { ctx with next_var = ctx.next_var + 1 } )


let over_context f ctx =
  (* print_string (List.to_string show_context_element @@ ctx.context); print_newline (); *)
  { ctx with context = f ctx.context }


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
            else (true, Context_row_evar_assignment (ev, r)) | ce ->
            (found, ce))
      false
      ctx
  in
  if found
  then ctx'
  else (
    print_ctx { empty_ctx with context = ctx } ;
    failwith @@ "solve_row_evar not found (" ^ Int.to_string ev ^ ")" )


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


(* Parallel to drop_ctx_from. Still removes the element queried. *)
let get_ctx_after ce =
  let rec go = function
    | ce' :: ces when Stdlib.(ce' = ce) ->
        ces
    | _ :: ces ->
        go ces
    | [] ->
        []
  in
  over_context go


let free_evars ctx =
  let match_evar = function Context_evar ev -> Some ev | _ -> None in
  List.filter_map match_evar ctx.context


let free_row_evars ctx =
  let match_evar = function Context_row_evar ev -> Some ev | _ -> None in
  List.filter_map match_evar ctx.context


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
    | Type.Record r ->
        Type.Record (row r)
    | Type.Function (t1, t2) ->
        Type.Function (go t1, go t2)
    | Type.EVar ev ->
        ctx.context
        |> List.find_map (function
               | Context_evar_assignment (ev', t) when Int.(ev = ev') ->
                   Some (go t)
               | _ ->
                   None)
        |> Option.get_or ~default:(Type.EVar ev)
    | Type.Forall (tv, tp) ->
        Type.Forall (tv, go tp)
    | Type.ForallRow (tv, tp) ->
        Type.ForallRow (tv, go tp)
    | Type.Mu (tv, tp) ->
        Type.Mu (tv, go tp)
    | Type.Primitive p ->
        Type.Primitive p
    | Type.TVar tv ->
        Type.TVar tv
    | Type.Variant () ->
        Type.Variant ()
  and row (l, t) =
    let l' = List.map (Pair.map2 go) l in
    match t with
    | Some (Tail_evar ev) ->
        ctx.context
        |> List.find_map (function
               | Context_row_evar_assignment (ev', r) when Int.(ev = ev') ->
                   (* TODO recursively apply context to the row here *)
                   Some r
               | _ ->
                   None)
        |> Option.map_or ~default:(l', t) (fun (l'', t') -> (l' @ l'', t'))
    | _ ->
        (l', t)
  in
  go


let apply_ctx_expr ctx =
  let open Ast in
  let rec go (annotated : Type.t Ast.Expr.t) : Type.t Ast.Expr.t =
    let tp = apply_ctx ctx annotated.tp in
    let expr =
      match annotated.expr with
      | Expr.Application (e1, e2) ->
          Expr.Application (go e1, go e2)
      | Expr.Function (v, e) ->
          Expr.Function (v, go e)
      | Expr.Record r ->
          Expr.Record (List.map (Pair.map2 go) r)
      | Expr.Variant (v, r) ->
          Expr.Variant (v, List.map (Pair.map2 go) r)
      | Expr.Var v ->
          Expr.Var v
      | Expr.Assign (v, e1, e2) ->
          Expr.Assign (v, go e1, go e2)
      | Expr.Projection (r, lbl) ->
          Expr.Projection (go r, lbl)
      | Expr.Extension (lbl, e, r) ->
          Expr.Extension (lbl, go e, go r)
      | Expr.Literal l ->
          Expr.Literal l
    in
    { expr; tp }
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
  @@ fun l ->
  List.fold_right
    (fun ce' acc ->
      ( if Stdlib.(ce' = ce) (* TODO get rid of polymorphic comparison *)
      then ces
      else [] )
      @ (ce' :: acc))
    l
    []


let append_ctx ces = over_context @@ fun context -> List.append context ces

(* TYPE FUNCTIONS *)

(* occurs_check:
   takes in an evar ev and a type tp.

   says whether ev occurs in tp.

   right now does not do occurs checks for row evars.
*)
let occurs_check ev =
  (* let check_tail = function
   *   | Some (Type.Tail_evar ev') when Int.(ev = ev') ->
   *       true
   *   | _ ->
   *       false
   * in *)
  let rec go = function
    | Type.Function (t1, t2) ->
        go t1 || go t2
    | Type.EVar ev' ->
        Int.(ev = ev')
    | Type.Record (r, _rt) ->
        List.exists (snd %> go) r
    | Type.Forall (_, tp) ->
        go tp
    | Type.ForallRow (_, tp) ->
        go tp
    | Type.Mu (_, tp) ->
        go tp
    | Type.TVar _ ->
        false
    | Type.Primitive _ ->
        false
    | Type.Variant () ->
        failwith "occurs_check: unimplemented"
  in
  go


(* substitute:
   takes in an int tv representing a tvar, a type replace_tp to replace it with, and a type tp.

   traverses the type tp and replaces all instances of TVar tv with replace_tp.
*)
let substitute tv ~replace_with =
  let rec go = function
    | Type.Record r ->
        Type.Record (Pair.map1 (List.map @@ Pair.map2 go) r)
    | Type.Variant () ->
        Type.Variant ()
    | Type.Function (t1, t2) ->
        Type.Function (go t1, go t2)
    | Type.EVar ev ->
        Type.EVar ev
    | Type.TVar tv' ->
        if String.(equal tv tv') then replace_with else Type.TVar tv'
    | Type.Forall (tv', tp) ->
        if String.(equal tv tv')
        then Type.Forall (tv', tp)
        else Type.Forall (tv', go tp)
    | Type.ForallRow (tv', tp) ->
        if String.(equal tv tv')
        then Type.ForallRow (tv', tp)
        else Type.ForallRow (tv', go tp)
    | Type.Mu (tv', tp) ->
        if String.(equal tv tv')
        then Type.ForallRow (tv', tp)
        else Type.ForallRow (tv', go tp)
    | Type.Primitive p ->
        Type.Primitive p
  in
  go


let substitute_row tv ~replace_with =
  let rec go = function
    | Type.Record (r, rt) ->
        let rt' =
          match rt with
          | Some (Type.Tail_tvar tv') when String.(equal tv' tv) ->
              Some replace_with
          | _ ->
              None
        in
        Type.Record ((List.map @@ Pair.map2 go) r, rt')
    | Type.Variant () ->
        Type.Variant ()
    | Type.Function (t1, t2) ->
        Type.Function (go t1, go t2)
    | Type.EVar ev ->
        Type.EVar ev
    | Type.TVar tv' ->
        Type.TVar tv'
    | Type.Forall (tv', tp) ->
        if String.(equal tv tv')
        then Type.Forall (tv', tp)
        else Type.Forall (tv', go tp)
    | Type.ForallRow (tv', tp) ->
        if String.(equal tv tv')
        then Type.ForallRow (tv', tp)
        else Type.ForallRow (tv', go tp)
    | Type.Mu (tv', tp) ->
        if String.(equal tv tv') then Type.Mu (tv', tp) else Type.Mu (tv', go tp)
    | Type.Primitive p ->
        Type.Primitive p
  in
  go


(* could take a list as input to avoid multiple traversals *)
let substitute_evar ev ~replace_with =
  let rec go = function
    | Type.Record r ->
        Type.Record (Pair.map1 (List.map @@ Pair.map2 go) r)
    | Type.Variant () ->
        Type.Variant ()
    | Type.Function (t1, t2) ->
        Type.Function (go t1, go t2)
    | Type.EVar ev' ->
        if Int.(ev' = ev) then replace_with else Type.EVar ev'
    | Type.TVar tv ->
        Type.TVar tv
    | Type.Forall (tv, tp) ->
        (* It's probably OK since we make unique identifiers, but we probably
           should check if we're replacing with a TVar and avoid going into the
           body of the Forall if the TVars match. *)
        Type.Forall (tv, go tp)
    | Type.ForallRow (tv, tp) ->
        (* See above *)
        Type.ForallRow (tv, go tp)
    | Type.Mu (tv, tp) ->
        (* See above *)
        Type.Mu (tv, go tp)
    | Type.Primitive p ->
        Type.Primitive p
  in
  go


let substitute_row_evar ev ~replace_with =
  let rec go = function
    | Type.Record (r, rt) ->
        let rt' =
          match rt with
          | Some (Tail_evar ev') when Int.(equal ev' ev) ->
              Some replace_with
          | _ ->
              None
        in
        Type.Record ((List.map @@ Pair.map2 go) r, rt')
    | Type.Variant () ->
        Type.Variant ()
    | Type.Function (t1, t2) ->
        Type.Function (go t1, go t2)
    | Type.EVar ev' ->
        Type.EVar ev'
    | Type.TVar tv ->
        Type.TVar tv
    | Type.Forall (tv, tp) ->
        (* It's probably OK since we make unique identifiers, but we probably
           should check if we're replacing with a TVar and avoid going into the
           body of the Forall if the TVars match. *)
        Type.Forall (tv, go tp)
    | Type.ForallRow (tv, tp) ->
        (* See above *)
        Type.ForallRow (tv, go tp)
    | Type.Mu (tv, tp) ->
        (* See above *)
        Type.Mu (tv, go tp)
    | Type.Primitive p ->
        Type.Primitive p
  in
  go


let unfold_mu (tp : Type.t) : Type.t =
  match tp with
  | Mu (tv, mu_inner) ->
      substitute tv ~replace_with:tp mu_inner
  | _ ->
      failwith @@ "unfold_mu: got unexpected type (type should only be Mu)."


let make_recursive ev (tp : Type.t) : Type.t =
  let tv = "t" ^ Int.to_string ev in
  print_tp @@ Type.Mu (tv, substitute_evar ev ~replace_with:(Type.TVar tv) tp) ;
  Type.Mu (tv, substitute_evar ev ~replace_with:(Type.TVar tv) tp)


(* quantifies over all the given evars *)

let quantify (evars : int list) (tp : Type.t) : Type.t =
  let quantify_single ev tp' =
    let tv = "a" ^ Int.to_string ev in
    Type.Forall (tv, substitute_evar ev ~replace_with:(Type.TVar tv) tp')
  in
  List.fold_right quantify_single evars tp


let quantify_row (row_evars : int list) (tp : Type.t) : Type.t =
  let quantify_single ev tp' =
    let tv = "a" ^ Int.to_string ev in
    Type.ForallRow
      (tv, substitute_row_evar ev ~replace_with:(Type.Tail_tvar tv) tp')
  in
  List.fold_right quantify_single row_evars tp


(* quantifies both row and type evars *)

let quantify_all_free_evars ctx tp =
  let fvs = free_evars ctx in
  let row_fvs = free_row_evars ctx in
  quantify_row row_fvs @@ quantify fvs tp


(* INFERENCE and CHECKING *)

let rec infer_top ctx annotated =
  let inferred, new_ctx = infer ctx annotated in
  let resolved = apply_ctx_expr new_ctx inferred in
  ({ resolved with tp = quantify (free_evars new_ctx) resolved.tp }, new_ctx)


and infer ctx (annotated : Ast.Expr.Untyped.t) : Type.t Ast.Expr.t * context =
  print_string "infer: " ;
  print_ctx ctx ;
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
            Record
              (List.map (Pair.map2 (fun e -> e.Ast.Expr.tp)) inferred_rcd, None)
        }
      , new_ctx )
  (* ->I => *)
  | Ast.Expr.Function (var, e) ->
      let arg_ev_tp, arg_ev_ce, _, ctx' = fresh_evar ctx in
      let ret_ev_tp, ret_ev_ce, _, ctx'' = fresh_evar ctx' in
      let marker = Context_marker arg_ev_ce in
      let fun_ctx =
        append_ctx
          [ marker; arg_ev_ce; ret_ev_ce; Context_var (var, arg_ev_tp) ]
          ctx''
      in
      let e_checked, new_ctx = check fun_ctx e ret_ev_tp in
      let fun_expr =
        apply_ctx_expr
          new_ctx
          { Ast.Expr.expr = Ast.Expr.Function (var, e_checked)
          ; Ast.Expr.tp = Function (arg_ev_tp, ret_ev_tp)
          }
      in
      let quantified_fun_expr =
        { fun_expr with
          tp =
            quantify_all_free_evars (get_ctx_after marker new_ctx) fun_expr.tp
        }
      in
      (quantified_fun_expr, drop_ctx_from marker new_ctx)
  (* ->E *)
  | Ast.Expr.Application (e1, e2) ->
      let e1_inferred, new_ctx = infer ctx e1 in
      let e1_inferred' = apply_ctx_expr new_ctx e1_inferred in
      let e2_inferred, output_tp, new_ctx' =
        infer_app new_ctx e1_inferred'.tp e2
      in
      ( { expr = Application (e1_inferred', e2_inferred); tp = output_tp }
      , new_ctx' )
  (* TODO *)
  | Ast.Expr.Variant _ ->
      ({ expr = Ast.Expr.Variant ("", []); tp = Variant () }, ctx)
  | Ast.Expr.Assign (var, e1, e2) ->
      (* TODO for assignments that involve updates: check if var is in the context. if it is, check e1 against its type. otherwise, infer the type of e1 - and use that as var's assignment. *)
      let e1_inferred, new_ctx = infer ctx e1 in
      let e1_inferred' = apply_ctx_expr new_ctx e1_inferred in
      let assign_ctx =
        append_ctx [ Context_var (var, e1_inferred'.tp) ] new_ctx
      in
      let e2_inferred, new_ctx' = infer assign_ctx e2 in
      ( { expr = Ast.Expr.Assign (var, e1_inferred', e2_inferred)
        ; tp = e2_inferred.tp
        }
      , new_ctx' )
  | Ast.Expr.Projection (e, lbl) ->
      let e_inferred, new_ctx = infer ctx e in
      let e_inferred' = apply_ctx_expr new_ctx e_inferred in
      let proj_tp, proj_ctx = infer_proj new_ctx e_inferred'.tp lbl in
      ({ expr = Ast.Expr.Projection (e_inferred', lbl); tp = proj_tp }, proj_ctx)
  | Ast.Expr.Extension (lbl, e1, e2) ->
      let e1_inferred, new_ctx = infer ctx e1 in
      let e2_inferred, new_ctx' = infer new_ctx e2 in
      let e2_inferred' = apply_ctx_expr new_ctx' e2_inferred in
      let ext_tp, ext_ctx =
        infer_ext new_ctx' (lbl, e1_inferred.tp) e2_inferred'.tp
      in
      ( { expr = Ast.Expr.Extension (lbl, e1_inferred, e2_inferred')
        ; tp = ext_tp
        }
      , ext_ctx )
  | Ast.Expr.Literal l ->
      ( { expr = Ast.Expr.Literal l
        ; tp = Primitive (match l with Ast.Expr.Int _ -> Int)
        }
      , ctx )


and check ctx annotated tp =
  print_string "check: " ;
  print_tp tp ;
  print_ctx ctx ;
  let open Ast in
  match (annotated.expr, tp) with
  (* forall I *)
  | _, Forall (tv, tp') ->
      let ctx' = append_ctx [ Context_tvar tv ] ctx in
      let checked_e, new_ctx = check ctx' annotated tp' in
      ( { checked_e with tp = Forall (tv, checked_e.tp) }
      , drop_ctx_from (Context_tvar tv) new_ctx )
  (* forallRow I *)
  | _, ForallRow (tv, tp') ->
      let ctx' = append_ctx [ Context_row_tvar tv ] ctx in
      let checked_e, new_ctx = check ctx' annotated tp' in
      ( { checked_e with tp = ForallRow (tv, checked_e.tp) }
      , drop_ctx_from (Context_row_tvar tv) new_ctx )
  (* -> I *)
  | Expr.Function (var, e), Function (arg_tp, return_tp) ->
      let fun_ctx = append_ctx [ Context_var (var, arg_tp) ] ctx in
      let checked_e, new_ctx = check fun_ctx e return_tp in
      ( { expr = Function (var, checked_e); tp = Function (arg_tp, return_tp) }
      , drop_ctx_from (Context_var (var, arg_tp)) new_ctx )
  (* <= Mu *)
  | _, (Mu _ as mu) ->
      check ctx annotated (unfold_mu mu)
  (* Sub *)
  | _ ->
      let e_inferred, ctx' = infer ctx annotated in
      let e_inferred' = apply_ctx_expr ctx' e_inferred in
      let new_ctx = subsumes ctx' e_inferred'.tp (apply_ctx ctx' tp) in
      (* do we do { e_inferred with tp } ? *)
      (e_inferred', new_ctx)


and infer_app ctx tp e1 =
  print_string "infer_app: " ;
  print_tp tp ;
  print_ctx ctx ;
  match tp with
  (* Forall App *)
  | Forall (tv, forall_inner) ->
      let ev_tp, ev_ce, _, ctx' = fresh_evar ctx in
      let subst_ctx = append_ctx [ ev_ce ] ctx' in
      let subst_forall_inner = substitute tv ~replace_with:ev_tp forall_inner in
      infer_app subst_ctx subst_forall_inner e1
  (* Forall row App *)
  | ForallRow (tv, forall_inner) ->
      let ev_tail, ev_ce, _, ctx' = fresh_row_evar ctx in
      let subst_ctx = append_ctx [ ev_ce ] ctx' in
      let subst_forall_inner =
        substitute_row tv ~replace_with:ev_tail forall_inner
      in
      infer_app subst_ctx subst_forall_inner e1
  (* -> App *)
  | Function (arg_tp, return_tp) ->
      let checked_e1, new_ctx = check ctx e1 arg_tp in
      (checked_e1, return_tp, new_ctx)
  (* EVar App *)
  | EVar ev ->
      let arg_ev_tp, arg_ev_ce, _, ctx' = fresh_evar ctx in
      let ret_ev_tp, ret_ev_ce, _, ctx'' = fresh_evar ctx' in
      (* The EVar should be unsovled if we find it, so it's safe to use 'Context_evar ev' *)
      let inserted_ctx =
        insert_before_in_ctx (Context_evar ev) [ arg_ev_ce; ret_ev_ce ] ctx''
      in
      let solved_ctx =
        solve_evar ev (Function (arg_ev_tp, ret_ev_tp)) inserted_ctx
      in
      let checked_e1, new_ctx = check solved_ctx e1 arg_ev_tp in
      (checked_e1, ret_ev_tp, new_ctx)
  | Mu (_, _) as mu ->
      infer_app ctx (unfold_mu mu) e1
  | _ ->
      failwith "infer_app: Got unexpected type."


and infer_proj ctx tp lbl =
  match tp with
  (* Forall Prj *)
  | Forall (tv, forall_inner) ->
      let ev_tp, ev_ce, _, ctx' = fresh_evar ctx in
      let subst_ctx = append_ctx [ ev_ce ] ctx' in
      let subst_forall_inner = substitute tv ~replace_with:ev_tp forall_inner in
      infer_proj subst_ctx subst_forall_inner lbl
  (* Forall Row Prj *)
  | ForallRow (tv, forall_inner) ->
      let ev_tail, ev_ce, _, ctx' = fresh_row_evar ctx in
      let subst_ctx = append_ctx [ ev_ce ] ctx' in
      let subst_forall_inner =
        substitute_row tv ~replace_with:ev_tail forall_inner
      in
      infer_proj subst_ctx subst_forall_inner lbl
  (* Rcd Prj *)
  | tp ->
      lookup_row ctx tp lbl


and lookup_row ctx tp lbl =
  match tp with
  (* LookupRcd *)
  | Record (r, rt) ->
    ( match
        List.find_map
          (function
            | lbl', tp when String.(equal lbl lbl') -> Some tp | _ -> None)
          r
      with
    | Some tp ->
        (tp, ctx)
    | None ->
      ( match rt with
      | Some (Tail_evar ev) ->
          let fresh_rt, fresh_rt_ce, _, ctx1 = fresh_row_evar ctx in
          let fresh_ev_tp, fresh_ev_ce, _, ctx2 = fresh_evar ctx1 in
          let ctx3 =
            insert_before_in_ctx
              (Context_row_evar ev)
              [ fresh_ev_ce; fresh_rt_ce ]
              ctx2
          in
          let ctx4 =
            solve_row_evar ev ([ (lbl, fresh_ev_tp) ], Some fresh_rt) ctx3
          in
          (fresh_ev_tp, ctx4)
      | _ ->
          failwith @@ "lookup_row: " ^ lbl ^ " not found." ) )
  (* LookupEVar *)
  | EVar ev ->
      let fresh_rt, fresh_rt_ce, _, ctx1 = fresh_row_evar ctx in
      let fresh_ev_tp, fresh_ev_ce, _, ctx2 = fresh_evar ctx1 in
      let ctx3 =
        insert_before_in_ctx (Context_evar ev) [ fresh_ev_ce; fresh_rt_ce ] ctx2
      in
      let ctx4 =
        solve_evar ev (Record ([ (lbl, fresh_ev_tp) ], Some fresh_rt)) ctx3
      in
      (fresh_ev_tp, ctx4)
  | _ ->
      failwith "lookup_row: Got unexpected type."


and infer_ext ctx rcd_head tp =
  match tp with
  | Forall (tv, forall_inner) ->
      let ev_tp, ev_ce, _, ctx' = fresh_evar ctx in
      let subst_ctx = append_ctx [ ev_ce ] ctx' in
      let subst_forall_inner = substitute tv ~replace_with:ev_tp forall_inner in
      infer_ext subst_ctx rcd_head subst_forall_inner
  | ForallRow (tv, forall_inner) ->
      let ev_tail, ev_ce, _, ctx' = fresh_row_evar ctx in
      let subst_ctx = append_ctx [ ev_ce ] ctx' in
      let subst_forall_inner =
        substitute_row tv ~replace_with:ev_tail forall_inner
      in
      infer_ext subst_ctx rcd_head subst_forall_inner
  (* TODO Need to replace the label if it already exists *)
  | Record (r, rt) ->
      (Type.Record (rcd_head :: r, rt), ctx)
  | EVar ev ->
      let fresh_rt, fresh_rt_ce, _, ctx1 = fresh_row_evar ctx in
      let rcd_tp = Type.Record ([], Some fresh_rt) in
      let ctx2 = insert_before_in_ctx (Context_evar ev) [ fresh_rt_ce ] ctx1 in
      let ctx3 = solve_evar ev rcd_tp ctx2 in
      (* This is like rcd_tp, but extended with the rcd_head given *)
      (Record ([ rcd_head ], Some fresh_rt), ctx3)
  | _ ->
      failwith "infer_ext: Got unexpected type."


and subsumes ctx tp1 tp2 =
  print_string "subsumes: " ;
  print_tp tp1 ;
  print_tp tp2 ;
  print_ctx ctx ;
  match (tp1, tp2) with
  (* EVar *)
  | EVar ev1, EVar ev2 when Int.(ev1 = ev2) ->
      ctx
  (* TVar *)
  | TVar tv1, TVar tv2 ->
      if String.(equal tv1 tv2)
      then ctx
      else
        failwith @@ "subsumes: Inequivalent tvars " ^ tv1 ^ " and " ^ tv2 ^ "."
  (* Forall L *)
  | Forall (tv, forall_inner), tp2 ->
      let ev_tp, ev_ce, _, ctx' = fresh_evar ctx in
      let marker = Context_marker ev_ce in
      let subst_ctx = append_ctx [ marker; ev_ce ] ctx' in
      let subst_forall_inner = substitute tv ~replace_with:ev_tp forall_inner in
      subsumes subst_ctx subst_forall_inner tp2 |> drop_ctx_from marker
  (* Forall Row L *)
  | ForallRow (tv, forall_inner), tp2 ->
      let ev_tail, ev_ce, _, ctx' = fresh_row_evar ctx in
      let marker = Context_marker ev_ce in
      let subst_ctx = append_ctx [ marker; ev_ce ] ctx' in
      let subst_forall_inner =
        substitute_row tv ~replace_with:ev_tail forall_inner
      in
      subsumes subst_ctx subst_forall_inner tp2 |> drop_ctx_from marker
  (* Forall R *)
  | tp1, Forall (tv, forall_inner) ->
      let ctx' = append_ctx [ Context_tvar tv ] ctx in
      subsumes ctx' tp1 forall_inner |> drop_ctx_from (Context_tvar tv)
  (* Forall Row R *)
  | tp1, ForallRow (tv, forall_inner) ->
      let ctx' = append_ctx [ Context_row_tvar tv ] ctx in
      subsumes ctx' tp1 forall_inner |> drop_ctx_from (Context_row_tvar tv)
  (* Mu *)
  | Mu (tv1, mu_inner1), Mu (tv2, mu_inner2) ->
      (* This might be really messed up... *)
      (* Our inductive hypothesis is that inner expansions of the Mu are equal. tv1 and tv2 stand for these inner expansions, so our inductive hypothesis can be stated as tv1 unifying with tv2 whenever the two meet. So we can hack it by just replacing tv2 with tv1 in mu_inner2. *)
      let mu_inner2_ihop = substitute tv2 ~replace_with:(TVar tv1) mu_inner2 in
      subsumes ctx mu_inner1 mu_inner2_ihop
  | tp1, (Mu _ as mu2) ->
      subsumes ctx tp1 (unfold_mu mu2)
  | (Mu _ as mu1), tp2 ->
      subsumes ctx (unfold_mu mu1) tp2
  (* -> *)
  | Function (arg_tp1, return_tp1), Function (arg_tp2, return_tp2) ->
      let ctx' = subsumes ctx arg_tp2 arg_tp1 in
      subsumes ctx' (apply_ctx ctx' return_tp1) (apply_ctx ctx' return_tp2)
  (* Record *)
  | Record (r1, tail1), Record (r2, tail2) ->
      let map1 = RowMap.of_list r1 in
      let map2 = RowMap.of_list r2 in
      (* Check that the matching labels' types subsume each other *)
      let ctx' =
        RowMap.fold
          (fun _ (t1, t2) ctx -> subsumes ctx t1 t2)
          (row_map_zip map1 map2)
          ctx
      in
      let missingFrom1 = RowMap.to_list @@ row_map_difference map2 map1 in
      let missingFrom2 = RowMap.to_list @@ row_map_difference map1 map2 in
      (* Deal with the parts that are missing *)
      row_tail_subsumes ctx' tail1 missingFrom1 tail2 missingFrom2
  (* InstantiateL *)
  | EVar ev1, tp2 ->
      let tp2' = if occurs_check ev1 tp2 then make_recursive ev1 tp2 else tp2 in
      instantiateL ctx ev1 tp2'
  (* InstantiateR *)
  | tp1, EVar ev2 ->
      let tp1' = if occurs_check ev2 tp1 then make_recursive ev2 tp1 else tp1 in
      instantiateR ctx tp1' ev2
  | _ ->
      failwith "subsumes: unimplemented types"


and instantiateL ctx ev tp =
  print_string @@ "instantiateL: ev" ^ Int.to_string ev ;
  print_tp tp ;
  print_ctx ctx ;
  match tp with
  (* InstLArr *)
  | Function (arg_tp, ret_tp) ->
      let arg_ev_tp, arg_ev_ce, arg_ev, ctx' = fresh_evar ctx in
      let ret_ev_tp, ret_ev_ce, ret_ev, ctx'' = fresh_evar ctx' in
      (* The EVar should be unsovled if we find it, so it's safe to use 'Context_evar ev' *)
      let inserted_ctx =
        insert_before_in_ctx (Context_evar ev) [ arg_ev_ce; ret_ev_ce ] ctx''
      in
      let solved_ctx =
        solve_evar ev (Function (arg_ev_tp, ret_ev_tp)) inserted_ctx
      in
      let new_ctx = instantiateR solved_ctx arg_tp arg_ev in
      instantiateL new_ctx ret_ev (apply_ctx new_ctx ret_tp)
  (* InstLTVar *)
  | TVar tv ->
      solve_evar ev (TVar tv) ctx
  (* InstLPrim *)
  | Primitive p ->
      solve_evar ev (Primitive p) ctx
  (* InstLReach *)
  | EVar ev2 ->
      instantiateReach ctx ev ev2
  (* InstLRcd *)
  | Record (r, rt) ->
      let make_fresh_evars lst ctx =
        List.fold_right
          (fun _ (ev_tps, ev_ces, evs, ctx) ->
            let ev_tp, ev_ce, ev, ctx' = fresh_evar ctx in
            (ev_tp :: ev_tps, ev_ce :: ev_ces, ev :: evs, ctx'))
          lst
          ([], [], [], ctx)
      in
      let fresh_evar_tps, fresh_evar_ces, fresh_evs, ctx1 =
        make_fresh_evars r ctx
      in
      let fresh_rt, fresh_rt_ce, _, ctx2 = fresh_row_evar ctx1 in
      let ctx3 =
        insert_before_in_ctx
          (Context_evar ev)
          (fresh_evar_ces @ [ fresh_rt_ce ])
          ctx2
      in
      let ctx4 =
        List.fold_right2
          (fun (_, tp) ev ctx -> instantiateL ctx ev tp)
          r
          fresh_evs
          ctx3
      in
      let ctx5 =
        solve_evar
          ev
          (Record
             ( List.map2 (fun (lbl, _) ev_tp -> (lbl, ev_tp)) r fresh_evar_tps
             , Some fresh_rt ))
          ctx4
      in
      row_tail_subsumes ctx5 (Some fresh_rt) [] rt []
  (* InstLAllR *)
  | Forall (tv, forall_inner) ->
      let ctx' = append_ctx [ Context_tvar tv ] ctx in
      instantiateL ctx' ev forall_inner |> drop_ctx_from (Context_tvar tv)
  (* InstLAllRowR *)
  | ForallRow (tv, forall_inner) ->
      let ctx' = append_ctx [ Context_row_tvar tv ] ctx in
      instantiateL ctx' ev forall_inner |> drop_ctx_from (Context_row_tvar tv)
  (* InstMuR *)
  | Mu (tv, mu_inner) ->
      let mu_ev_tp, mu_ev_ce, mu_ev, ctx' = fresh_evar ctx in
      let inserted_ctx =
        insert_before_in_ctx (Context_evar ev) [ mu_ev_ce ] ctx'
      in
      let solved_ctx = solve_evar ev (Mu (tv, mu_ev_tp)) inserted_ctx in
      instantiateR solved_ctx mu_inner mu_ev
  | Variant () ->
      failwith "instantiateL: variant unimplemented"


and instantiateR ctx tp ev =
  print_string @@ "instantiateR: ev" ^ Int.to_string ev ;
  print_tp tp ;
  print_ctx ctx ;
  match tp with
  (* InstRArr *)
  | Function (arg_tp, ret_tp) ->
      let arg_ev_tp, arg_ev_ce, arg_ev, ctx' = fresh_evar ctx in
      let ret_ev_tp, ret_ev_ce, ret_ev, ctx'' = fresh_evar ctx' in
      (* The EVar should be unsovled if we find it, so it's safe to use 'Context_evar ev' *)
      let inserted_ctx =
        insert_before_in_ctx (Context_evar ev) [ arg_ev_ce; ret_ev_ce ] ctx''
      in
      let solved_ctx =
        solve_evar ev (Function (arg_ev_tp, ret_ev_tp)) inserted_ctx
      in
      let new_ctx = instantiateL solved_ctx arg_ev arg_tp in
      instantiateR new_ctx (apply_ctx new_ctx ret_tp) ret_ev
  (* InstRReach *)
  | EVar ev2 ->
      instantiateReach ctx ev ev2
  (* InstRTVar *)
  | TVar tv ->
      solve_evar ev (TVar tv) ctx
  (* InstRPrim *)
  | Primitive p ->
      solve_evar ev (Primitive p) ctx
  (* InstRRcd *)
  | Record (r, rt) ->
      let make_fresh_evars lst ctx =
        List.fold_right
          (fun _ (ev_tps, ev_ces, evs, ctx) ->
            let ev_tp, ev_ce, ev, ctx' = fresh_evar ctx in
            (ev_tp :: ev_tps, ev_ce :: ev_ces, ev :: evs, ctx'))
          lst
          ([], [], [], ctx)
      in
      let fresh_evar_tps, fresh_evar_ces, fresh_evs, ctx1 =
        make_fresh_evars r ctx
      in
      let fresh_rt, fresh_rt_ce, _, ctx2 = fresh_row_evar ctx1 in
      let ctx3 =
        insert_before_in_ctx
          (Context_evar ev)
          (fresh_evar_ces @ [ fresh_rt_ce ])
          ctx2
      in
      let ctx4 =
        List.fold_right2
          (fun (_, tp) ev ctx -> instantiateR ctx tp ev)
          r
          fresh_evs
          ctx3
      in
      let ctx5 =
        solve_evar
          ev
          (Record
             ( List.map2 (fun (lbl, _) ev_tp -> (lbl, ev_tp)) r fresh_evar_tps
             , Some fresh_rt ))
          ctx4
      in
      row_tail_subsumes ctx5 (Some fresh_rt) [] rt []
  (* InstRAllL *)
  | Forall (tv, forall_inner) ->
      let forall_ev_tp, forall_ev_ce, _, ctx' = fresh_evar ctx in
      let ctx'' =
        append_ctx [ Context_marker forall_ev_ce; forall_ev_ce ] ctx'
      in
      instantiateR
        ctx''
        (substitute tv ~replace_with:forall_ev_tp forall_inner)
        ev
      |> drop_ctx_from (Context_marker forall_ev_ce)
  (* InstRAllRowL*)
  | ForallRow (tv, forall_inner) ->
      let ev_tail, ev_ce, _, ctx' = fresh_row_evar ctx in
      let ctx'' = append_ctx [ Context_marker ev_ce; ev_ce ] ctx' in
      instantiateR
        ctx''
        (substitute_row tv ~replace_with:ev_tail forall_inner)
        ev
      |> drop_ctx_from (Context_marker ev_ce)
  (* InstMuL *)
  | Mu (tv, mu_inner) ->
      let mu_ev_tp, mu_ev_ce, mu_ev, ctx' = fresh_evar ctx in
      let inserted_ctx =
        insert_before_in_ctx (Context_evar ev) [ mu_ev_ce ] ctx'
      in
      let solved_ctx = solve_evar ev (Mu (tv, mu_ev_tp)) inserted_ctx in
      instantiateL solved_ctx mu_ev mu_inner
  | Variant () ->
      failwith "instantiateR: variant unimplemented"


(* find where ev1 and ev2 are located in the context. Assign the later one to the earlier one.*)
and instantiateReach ctx ev1 ev2 =
  let find_ev ev =
    List.find_idx
      (function Context_evar ev' -> Int.(ev = ev') | _ -> false)
      ctx.context
  in
  let ev1_index = find_ev ev1 in
  let ev2_index = find_ev ev2 in
  match (ev1_index, ev2_index) with
  | Some (ev1_i, _), Some (ev2_i, _) ->
      if ev1_i <= ev2_i
      then solve_evar ev2 (EVar ev1) ctx
      else solve_evar ev1 (EVar ev2) ctx
  | _ ->
      failwith
      @@ "instantiateReach: evar not in context. "
      ^ "ev1 ("
      ^ Int.to_string ev1
      ^ ") is "
      ^ Option.map_or
          ~default:"not in context"
          (fun _ -> "in context")
          ev1_index
      ^ " and ev2 ("
      ^ Int.to_string ev2
      ^ ") is "
      ^ Option.map_or
          ~default:"not in context"
          (fun _ -> "in context")
          ev2_index
      ^ " "
      ^ List.to_string show_context_element ctx.context


and row_tail_subsumes ctx tail1 missingFrom1 tail2 missingFrom2 =
  match (tail1, missingFrom1, tail2, missingFrom2) with
  | Some (Tail_tvar tv1), [], Some (Tail_tvar tv2), []
    when String.(equal tv1 tv2) ->
      ctx
  | Some (Tail_evar ev1), _, Some (Tail_evar ev2), _ ->
      row_tail_subsumes_ev ctx ev1 missingFrom1 ev2 missingFrom2
  (* The second tail must not be an evar otherwise the reach case would match, thus it must have no missing elements to add to it.  *)
  | Some (Tail_evar ev1), _, _, [] ->
      solve_row_evar ev1 (missingFrom1, tail2) ctx
  (* The first tail must not be an evar otherwise the reach case would match, thus it must have no missing elements to add to it. *)
  | _, [], Some (Tail_evar ev2), _ ->
      solve_row_evar ev2 (missingFrom2, tail1) ctx
  | _ ->
      failwith "row_tail_subsumes: invalid case"


and row_tail_subsumes_ev ctx ev1 missingFrom1 ev2 missingFrom2 =
  let make_fresh_evars lst ctx =
    List.fold_right
      (fun _ (ev_tps, ev_ces, evs, ctx) ->
        let ev_tp, ev_ce, ev, ctx' = fresh_evar ctx in
        (ev_tp :: ev_tps, ev_ce :: ev_ces, ev :: evs, ctx'))
      lst
      ([], [], [], ctx)
  in
  let fresh_evars1, fresh_evars1_ces, fresh_evs1, ctx1 =
    make_fresh_evars missingFrom1 ctx
  in
  let fresh_evars2, fresh_evars2_ces, fresh_evs2, ctx2 =
    make_fresh_evars missingFrom2 ctx1
  in
  let fresh_rt1, fresh_rt1_ce, fresh_rev1, ctx3 = fresh_row_evar ctx2 in
  let fresh_rt2, fresh_rt2_ce, fresh_rev2, ctx4 = fresh_row_evar ctx3 in
  let ctx5 =
    insert_before_in_ctx
      (Context_row_evar ev1)
      (fresh_evars1_ces @ [ fresh_rt1_ce ])
      ctx4
    |> insert_before_in_ctx
         (Context_row_evar ev2)
         (fresh_evars2_ces @ [ fresh_rt2_ce ])
  in
  let ctx6 =
    solve_row_evar
      ev1
      ( List.map2 (fun (lbl, _) ev_tp -> (lbl, ev_tp)) missingFrom1 fresh_evars1
      , Some fresh_rt1 )
      ctx5
  in
  let ctx7 =
    solve_row_evar
      ev2
      ( List.map2 (fun (lbl, _) ev_tp -> (lbl, ev_tp)) missingFrom2 fresh_evars2
      , Some fresh_rt2 )
      ctx6
  in
  let ctx8 =
    List.fold_right2
      (fun (_, tp) ev ctx -> instantiateL ctx ev tp)
      missingFrom1
      fresh_evs1
      ctx7
  in
  let ctx9 =
    List.fold_right2
      (fun (_, tp) ev ctx -> instantiateR ctx tp ev)
      missingFrom2
      fresh_evs2
      ctx8
  in
  row_tail_reach ctx9 fresh_rev1 fresh_rev2


and row_tail_reach ctx ev1 ev2 =
  let find_ev ev =
    List.find_idx
      (function Context_row_evar ev' -> Int.(ev = ev') | _ -> false)
      ctx.context
  in
  let ev1_index = find_ev ev1 in
  let ev2_index = find_ev ev2 in
  match (ev1_index, ev2_index) with
  | Some (ev1_i, _), Some (ev2_i, _) ->
      if ev1_i <= ev2_i
      then solve_row_evar ev2 ([], Some (Tail_evar ev1)) ctx
      else solve_row_evar ev1 ([], Some (Tail_evar ev2)) ctx
  | _ ->
      failwith "row_tail_reach: row evar not in context."
