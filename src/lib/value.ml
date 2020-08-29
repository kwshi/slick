type primitive = Int of Z.t

type t =
  | Record of (string * t) list
  | Function of (t -> t)
  | Variant of string * (string * t) list
  | Primitive of primitive

let rec pp ppf value =
  let open Fmt in
  ( match value with
  | Record r ->
      const pp_record r
  | Function _ ->
      any "<function>"
  | Variant (v, r) ->
      const string v ++ const pp_record r
  | Primitive p ->
    (match p with Int n -> const Z.pp_print n) )
    ppf
    ()


and pp_record ppf =
  let open Fmt in
  ( any "{"
  ++ list ~sep:(any ",@ ") (pair ~sep:(any "@ =@ ") string pp)
  ++ any "}" )
    ppf
