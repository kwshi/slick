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

module Expr = struct
  type literal =
    | String of string
    | Int of Z.t

  type pattern =
    | Tag_pat of (label * var_name)
    | Var_pat of var_name

  type 't raw_expr =
    | Bop of string * 't * 't
    | Assign of var_name * 't * 't
    | Function of (var_name * 't)
    | Application of ('t * 't)
    | Record of 't record
    | Projection of ('t * label)
    | Extension of (label * 't * 't)
    | Variant of (string * 't)
    | Var of var_name
    | Literal of literal
    | Case of ('t * (pattern * 't) list)
  and 'annotated_expr record = (label * 'annotated_expr) list

  and 'tp t = {tp: 'tp; expr: 'tp t raw_expr}

  module Untyped = struct
    type 'a expr = 'a t

    type t = unit expr

    let make expr = {tp= (); expr}

    let make_function v e = make @@ Function (v, e)

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
  end
end

module Module = struct
  type 'tp t = (string * 'tp Expr.t) list
end

module Repl = struct
  type 'tp t = 
    | Def of string * 'tp Expr.t
    | Expr of 'tp Expr.t
end
