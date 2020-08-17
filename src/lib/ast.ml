open Containers 

exception SyntaxError of string

type type_ =
  | Type_record of record_type
  | Type_variant of unit (* TODO *)
  | Type_function of (record_type * type_)
  | Type_uvar of int
[@@deriving show]

and record_type = (string * type_) list [@@deriving show]

type var_name = string

let pp_var_name = Format.string

type label = string
let pp_label = Format.string

type 'annotated_expr expr =
  | Function of (record_type * 'annotated_expr)
  | Application of ('annotated_expr * 'annotated_expr)
  | Record of 'annotated_expr record
  | Variant of (string * 'annotated_expr record)
  | Var of var_name
[@@deriving show]

and 'annotated_expr record = (label * 'annotated_expr) list [@@deriving show]

and 'tp annotated_expr =
  { tp : 'tp
  ; expr : 'tp annotated_expr expr
  }
[@@deriving show]

module UntypedExpr = struct
  type t = unit annotated_expr

  let make expr = { tp = (); expr }

  let make_function r e = make @@ Function (r, e)

  let make_application e1 e2 = make @@ Application (e1, e2)

  let make_record r = make @@ Record r

  let make_variant s r = make @@ Variant (s, r)

  let make_var v = make @@ Var v
end
