module Pattern : sig
  type literal =
    | String of string
    | Int of Z.t

  type t =
    | Record of (Label.t * t) list
    | Variant of (Label.t * t)
    | Var of Var_name.t
    | Literal of literal
end

module Expr : sig
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
    | Tuple of 't tuple

  and 'annotated_expr record = (Label.t * 'annotated_expr) list

  and 't tuple = 
    { unlabeled : 't list
    ; labeled : (Label.t * 't) list
    }

  and 'tp t =
    { tp : 'tp
    ; expr : 'tp t raw_expr
    }

  module Untyped : sig
    type 'a expr := 'a t

    type t = unit expr

    val make : t raw_expr -> t

    val make_function : Pattern.t -> t -> t

    val make_function_curried : Pattern.t list -> t -> t

    val make_application : t -> t -> t

    val make_record : t record -> t

    val make_tuple : t list -> (Label.t * t) list -> t

    val make_projection : t -> Label.t -> t

    val make_extension : t -> string * t -> t

    val make_extensions : t -> (string * t) list -> t

    val make_variant : string -> t -> t

    val make_var : string -> t 

    val make_assign : string -> t -> t -> t

    val make_literal : literal -> t

    val make_case : t -> (Pattern.t * t) list -> t

    val make_bop : string -> t -> t -> t

    val make_uop : string -> t -> t
  end
end

module Module : sig
  type 'a t = (string * 'a Expr.t) list

  val make : (string * 'a Expr.t) list -> 'a t
end

module Repl : sig
  type 'tp t =
    | Empty
    | Def of string * 'tp Expr.t
    | Expr of 'tp Expr.t
    | Cmd of string * string
end
