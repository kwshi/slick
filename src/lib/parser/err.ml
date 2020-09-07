type t =
  | Unexpected_char of char

exception Err of t

let pp ppf e =
  let open Fmt in
  (match e with
   | Unexpected_char c ->
     const text "unexpected character "
     ++ quote ~mark:"`" (const char c)
  )
  ppf ()
  
