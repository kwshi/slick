

exception SyntaxError of string

type type_ =
  | Type_record of record_type
  | Type_function of (record_type * type_)
[@@deriving show]
and record_type =
  (string * type_) list
[@@deriving show]



type 'annotated_expr expr =
  | Function of (record_type * 'annotated_expr)
  | Application of ('annotated_expr * 'annotated_expr record)
  | Record of 'annotated_expr record
  | Variant of (string * 'annotated_expr record)
[@@deriving show]
and 'annotated_expr record =
  (string * 'annotated_expr) list
[@@deriving show]
and 'tp annotated_expr =
  { tp: 'tp
  ; expr: 'tp annotated_expr expr
  }
[@@deriving show]

module UntypedExpr = struct
  
  type t = unit annotated_expr

  let make expr = {tp = (); expr}

  let make_function r e = make @@ Function (r, e)
  let make_application e r = make @@ Application (e, r)
  let make_record r = make @@ Record r
  let make_variant s r = make @@ Variant (s, r)

end


