open Containers

exception SyntaxError of string

type label = string

let pp_label = Format.string

type var_name = string

let pp_var_name = Format.string

(*
module Type = struct
  type t =
    | Record of record
    | Variant of unit (* TODO *)
    | Function of (record * t)

  and record = (string * t) list 
end

              *)

module Pattern = struct
  type literal =
    | String of string
    | Int of Z.t

  type t =
    | Record of (label * t) list
    | Variant of (label * t)
    | Var of var_name
    | Literal of literal

end


module Expr = struct
  type literal =
    | String of string
    | Int of Z.t

  type 't raw_expr =
    | Bop of string * 't * 't
    | Uop of string * 't
    | Assign of var_name * 't * 't
    | Function of (Pattern.t * 't)
    | Application of ('t * 't)
    | Record of 't record
    | Projection of ('t * label)
    | Extension of (label * 't * 't)
    | Variant of (string * 't)
    | Var of var_name
    | Literal of literal
    | Case of ('t * (Pattern.t * 't) list)
  and 'annotated_expr record = (label * 'annotated_expr) list

  and 'tp t = {tp: 'tp; expr: 'tp t raw_expr}

  module Untyped = struct
    type 'a expr = 'a t

    type t = unit expr

    let make expr = {tp= (); expr}

    let make_function v e = make @@ Function (v, e)

    let make_function_with_args args e = List.fold_right make_function args e

    let make_application e1 e2 = make @@ Application (e1, e2)

    let make_record r = make @@ Record r

    let make_projection r l = make @@ Projection (r, l)

    let make_extension r (k, v) = make @@ Extension (k, v, r)

    let make_extensions = List.fold_left make_extension

    let make_variant s r = make @@ Variant (s, r)

    let make_var v = make @@ Var v

    let make_assign v e b = make @@ Assign (v, e, b)

    let make_literal l = make @@ Literal l

    let make_case e cs = make @@ Case (e, cs)

    let make_bop o a b = make @@ Bop (o, a, b)

    let make_uop o a = make @@ Uop (o, a)
  end
end

module Module = struct
  type 'tp t = (string * 'tp Expr.t) list
end

module Repl = struct
  type 'tp t = 
    | Empty
    | Def of string * 'tp Expr.t
    | Expr of 'tp Expr.t
end

let rec match_pat =
  function
  | Pattern.Var var, v -> Some [(var, v)]
  | Pattern.Record fields, Val.Record r -> List.fold_right
    (fun (lbl, pat') accum_opt ->
    match accum_opt, Val.Record.get lbl r with
    | Some accum, Some v -> (match match_pat (pat', v) with
                            | Some bindings -> Some (bindings @ accum)
                            | None -> None)
    | _ -> None
    ) fields
    (Some [])
  | Pattern.Variant (lbl, pat'), (Val.Variant (lbl', v)) when String.(equal lbl lbl') ->
    match_pat (pat', v)
  | Pattern.Literal (Int i), Val.Primitive (Val.Primitive.Int i') when (Z.equal i i') ->
    Some []
  | Pattern.Literal (String s), Val.Primitive (Val.Primitive.String s') when String.(equal s s') ->
    Some []
  | _ -> None
