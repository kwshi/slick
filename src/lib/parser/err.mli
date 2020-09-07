type t =
  | Unexpected_char of char

exception Err of t

val pp : t Fmt.t
