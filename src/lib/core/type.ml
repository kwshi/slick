open Containers

type var_name = string

let pp_var_name = Format.string

type label = string

let pp_label = Format.string

type primitive =
  | String
  | Int

type tail =
  | Tail_evar of int
  | Tail_tvar of var_name

type t =
  | Record of row (* TODO: DEPRECATE *)
  | Tuple of row
  | Variant of row
  | Function of (t * t)
  | EVar of int
  | TVar of var_name
  | Forall of (var_name * t)
  | ForallRow of (var_name * t)
  | Mu of (var_name * t)
  | Primitive of primitive

and row = (label * t) list * tail option (* TODO: parametrize over label type? (int vs string) *)

let unit = Record ([], None) (* TODO: replace with tuple *)

let bool = Variant ([ ("True", unit); ("False", unit) ], None)

let pp_tail ppf tl =
  let open Fmt in
  ( match tl with
  | Tail_evar ev ->
      any "e" ++ const int ev
  | Tail_tvar tv ->
      const string tv )
    ppf
    ()


let rec pp ppf t =
  let open Fmt in
  ( match t with
  | Record r ->
      const pp_row r |> Format.within "{" "}"
  | Tuple r ->
      const pp_row r |> Format.within "(" ",)"
  | Variant r ->
      const pp_variant r
  | Function (a, b) ->
      any "(" ++ const pp a ++ any "@ ->@ " ++ const pp b ++ any ")"
  | EVar ev ->
      any "ε" ++ const int ev
  | TVar tv ->
      const string tv
  | Forall (_a, e) ->
      const pp e (* any "∀@ " ++ const string a ++ any ".@ " ++ const pp e *)
  | ForallRow (_a, e) ->
      const pp e (* any "ꓤ@ " ++ const string a ++ any ".@ " ++ const pp e *)
  | Mu (v, e) ->
      const string v ++ any "@ =@ (" ++ const pp e ++ any ")"
      (* any "µ@ " ++ const string v ++ any ".@ " ++ const pp e *)
  | Primitive p ->
    (match p with Int -> any "Int" | String -> any "String") )
    ppf
    ()


and pp_row ppf (es, tl) =
  let open Fmt in
  ( const (option ~none:nop (pp_tail ++ any "@ |@ ")) tl
  ++ const (list ~sep:comma (pair ~sep:(any "@ :@ ") string pp)) es )
    ppf
    ()


and pp_variant ppf (es, tl) =
  let open Fmt in
  ( any "⟦"
  ++ const (option ~none:nop (pp_tail ++ any "@ |@ ")) tl
  ++ const (list ~sep:comma pp_variant_entry) es
  ++ any "⟧" )
    ppf
    ()


and pp_variant_entry ppf (l, t) =
  let open Fmt in
  ( const string l
  ++ match t with Record ([], None) -> nop | _ -> any "@ :@ " ++ const pp t )
    ppf
    ()


let rmap f =
  let rec f' t = f ~go t
  and go t =
    match f' t with
    | Some t' ->
        t'
    | None ->
      ( match t with
      | Record r ->
          Record (row r)
      | Tuple r ->
          Tuple (row r)
      | Variant r ->
          Variant (row r)
      | Function (a, b) ->
          Function (go a, go b)
      | EVar ev ->
          EVar ev
      | TVar tv ->
          TVar tv
      | Forall (v, t) ->
          Forall (v, go t)
      | ForallRow (v, t) ->
          ForallRow (v, go t)
      | Mu (v, t) ->
          Mu (v, go t)
      | Primitive p ->
          Primitive p )
  and row (es, tl) = (List.map (Pair.map2 go) es, tl) in
  go


let fold f =
  let rec go acc t =
    let acc' = f acc t in
    match t with
    | Record (es, _) | Variant (es, _) | Tuple (es, _) ->
        List.fold_left (fun a (_, t) -> f a t) acc' es
    | Function (a, b) ->
        go (go acc' a) b
    | Forall (_, t) | ForallRow (_, t) | Mu (_, t) ->
        go acc' t
    | EVar _ | TVar _ | Primitive _ ->
        acc'
  in
  go


let map_tail f =
  let row go (r, t) = (List.map (Pair.map2 go) r, Option.map f t) in
  rmap (fun ~go ->
      let row = row go in
      function
      | Record r ->
          Some (Record (row r))
      | Variant r ->
          Some (Variant (row r))
      | _ ->
          None)
