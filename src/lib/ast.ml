

exception SyntaxError of string

type expr =
  | Function of (record * expr)
  | Application of (expr * expr)
  | Record of record
and record =
  (string * expr) list
