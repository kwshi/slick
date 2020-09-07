open Containers


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
    | Record of (Label.t * t) list
    | Variant of (Label.t * t)
    | Var of Var_name.t
    | Literal of literal
end

module Expr = struct
  type literal =
    | String of string
    | Int of Z.t

  type 't raw_expr =
    | Bop of string * 't * 't
    | Uop of string * 't
    | Assign of Var_name.t * 't * 't
    | Function of (Pattern.t * 't)
    | Application of ('t * 't)
    | Record of 't record
    | Projection of ('t * Label.t)
    | Extension of (Label.t * 't * 't)
    | Variant of (string * 't)
    | Var of Var_name.t
    | Literal of literal
    | Case of ('t * (Pattern.t * 't) list)

  and 'annotated_expr record = (Label.t * 'annotated_expr) list

  and 'tp t =
    { tp : 'tp
    ; expr : 'tp t raw_expr
    }

  module Untyped = struct
    type 'a expr = 'a t

    type t = unit expr

    let make expr = { tp = (); expr }

    let make_function pat expr = make @@ Function (pat, expr)

    let make_function_curried args e = List.fold_right make_function args e

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

  let make = Fun.id
end

module Repl = struct
  type 'tp t =
    | Empty
    | Def of string * 'tp Expr.t
    | Expr of 'tp Expr.t
    | Cmd of string * string
end
