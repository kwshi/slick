open Containers

exception SyntaxError of string

type var_name = string
let pp_var_name = Format.string

type label = string
let pp_label = Format.string

module Type = struct
  type t =
    | Record of record
    | Variant of unit (* TODO *)
    | Function of (record * t)
    | EVar of int
    | TVar of var_name
    | Forall of var_name * t
  [@@deriving show]
  and record = (string * t) list
  [@@deriving show]
end


module Expr = struct
  type 't raw_expr =
    | Function of (Type.record * 't)
    | Application of ('t * 't record)
    | Record of 't record
    | Variant of (string * 't record)
    | Var of var_name
  [@@deriving show]
  and 'annotated_expr record = (label * 'annotated_expr) list
  [@@deriving show]
  and 'tp t =
    { tp : 'tp
    ; expr : 'tp t raw_expr
    }
  [@@deriving show]

  module Untyped = struct
    type 'a expr = 'a t
    type t = unit expr

    let make expr = { tp = (); expr }
    let make_function r e = make @@ Function (r, e)
    let make_application e1 e2 = make @@ Application (e1, e2)
    let make_record r = make @@ Record r
    let make_variant s r = make @@ Variant (s, r)
    let make_var v = make @@ Var v
  end
end
