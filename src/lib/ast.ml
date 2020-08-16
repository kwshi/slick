

exception SyntaxError of string


type type_ =
  | Type_record of record_type
  | Type_function of (record_type * type_)
and record_type =
  (string * type_) list



type expr =
  | Function of (record_type * expr)
  | Application of (expr * expr)
  | Record of record
  | Variant of (string * record)
and record =
  (string * expr) list


