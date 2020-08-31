open Containers

type element =
  | Var of string * Type.t (* Ast.var_name instead of string? *)
  | Row_evar of int
  | Row_tvar of string
  | Evar of int
  | Tvar of string
  | Evar_assignment of int * Type.t
  | Row_evar_assignment of int * Type.row
  | Marker of element
[@@deriving show]

type t =
  { next_var: int
        (* this is for both evars and tvars - we could separate it though *)
  ; context: element list }

let empty = {next_var= 0; context= []}

let of_list context = {next_var= List.length context; context}

let fresh_evar ctx =
  ( Type.EVar ctx.next_var
  , Evar ctx.next_var
  , ctx.next_var
  , {ctx with next_var= ctx.next_var + 1} )

let fresh_row_evar ctx =
  ( Type.Tail_evar ctx.next_var
  , Row_evar ctx.next_var
  , ctx.next_var
  , {ctx with next_var= ctx.next_var + 1} )

let fresh_marker ctx =
  ( Marker (Row_evar ctx.next_var)
  , {ctx with next_var= ctx.next_var + 1} )

let over_context f ctx =
  (* print_string (List.to_string show_context_element @@ ctx.context); print_newline (); *)
  {ctx with context= f ctx.context}

let lookup_var v ctx =
  List.find_map
    (function Var (v', tp) when String.(equal v v') -> Some tp | _ -> None)
    ctx.context
  |> Option.get_lazy (fun () ->
         failwith @@ "lookup_var unbound var: " ^ v ^ ".")

(* TODO change to free_evars or row_evars over a type *)
let free_evars ctx =
  let match_evar = function Evar ev -> Some ev | _ -> None in
  List.filter_map match_evar ctx.context

let free_row_evars ctx =
  let match_evar = function Row_evar ev -> Some ev | _ -> None in
  List.filter_map match_evar ctx.context

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
      false ctx
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

(* apply_ctx:
   takes in a context ctx and a type tp.

   if there is an evar in tp that has an assignment in ctx, the evar is replaced with its assignment.
   Similar to substitute, but for evars and potentially more than one variable.

   example:

   ctx = Context_evar 1, Context_evar_assignment 2 {}
   tp  = {x: evar 1} -> evar 2

   apply_ctx ctx tp = {x: evar 1 } -> {}
*)
let rec apply_ctx ctx =
  let rec go = function
    | Type.Record r ->
        Type.Record (row r)
    | Type.Function (t1, t2) ->
        Type.Function (go t1, go t2)
    | Type.EVar ev ->
        ctx.context
        |> List.find_map (function
             | Evar_assignment (ev', t) when Int.(ev = ev') ->
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
    | Type.Variant r ->
      Type.Variant (row r)
  and row (l, t) =
    let l' = List.map (Pair.map2 go) l in
    match apply_ctx_tail ctx t with
    | `Solved r   ->
      let (l'', t') = row r in
      (l'' @ l', t')
    | `Unsolved t -> (l', t)
  in
  go

and apply_ctx_tail ctx t =
  match t with
  | Some (Tail_evar ev) ->
      ctx.context
      |> List.find_map (function
            | Row_evar_assignment (ev', r) when Int.(ev = ev') ->
                Some (`Solved r)
            | _ ->
                None)
      |> Option.get_or ~default:(`Unsolved t)
  | _ ->
      `Unsolved t


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
      | Expr.Variant (v, e) ->
          Expr.Variant (v, go e)
      | Expr.Var v ->
          Expr.Var v
      | Expr.Assign (v, e1, e2) ->
          Expr.Assign (v, go e1, go e2)
      | Expr.Projection (r, lbl) ->
          Expr.Projection (go r, lbl)
      | Expr.Extension (lbl, e, r) ->
          Expr.Extension (lbl, go e, go r)
      | Expr.Case (e, cs) ->
        Expr.Case (go e, List.map (fun (v, p, e) -> (v, p, go e)) cs)
      | Expr.Literal l ->
          Expr.Literal l
    in
    {expr; tp}
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
      ( if Stdlib.(ce' = ce) (* TODO get rid of polymorphic comparison *) then
        ces
      else [] )
      @ (ce' :: acc))
    l []

let append_ctx ces = over_context @@ fun context -> List.append context ces
