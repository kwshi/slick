open Containers

type var_name = string

let pp_var_name = Format.string

type label = string

let pp_label = Format.string

type primitive = Int

type tail = Tail_evar of int | Tail_tvar of var_name

type t =
  | Record of row
  | Variant of row
  | Function of (t * t)
  | EVar of int
  | TVar of var_name
  | Forall of (var_name * t)
  | ForallRow of (var_name * t)
  | Mu of (var_name * t)
  | Primitive of primitive

and row = (label * t) list * tail option

let pp_tail ppf tl =
  let open Fmt in
  ( match tl with
  | Tail_evar ev ->
      any "e" ++ const int ev
  | Tail_tvar tv ->
      const string tv )
    ppf ()

let rec pp ppf t =
  let open Fmt in
  ( match t with
  | Record r ->
    const pp_row r
    |> Format.within "{" "}"
  | Variant r ->
    const pp_row r
    |> Format.within "[|" "|]"
  | Function (a, b) ->
      any "(" ++ const pp a ++ any "@ ->@ " ++ const pp b ++ any ")"
  | EVar ev ->
      any "e" ++ const int ev
  | TVar tv ->
      const string tv
  | Forall (a, e) ->
      any "forall@ " ++ const string a ++ any ".@ " ++ const pp e
  | ForallRow (a, e) ->
      any "forall_row@ " ++ const string a ++ any ".@ " ++ const pp e
  | Mu (v, e) ->
      any "µ@ " ++ const string v ++ any ".@ " ++ const pp e
  | Primitive p -> (
    match p with Int -> any "Int" ) )
    ppf ()

and pp_row ppf (es, tl) =
  let open Fmt in
  ( const (option ~none:nop (pp_tail ++ any "@ |@ ")) tl
  ++ const (list ~sep:comma (pair ~sep:(any "@ :@ ") string pp)) es
)
    ppf ()
