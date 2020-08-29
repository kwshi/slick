open Containers
open Fun

module Primitive = struct
  type t = Int of Z.t

  let pp ppf p =
    let open Fmt in
    (match p with Int n -> const Z.pp_print n) ppf ()

  module Get = struct
    let int = function Int n -> Some n

    module Exn = struct
      let int = Option.get_exn % int
    end
  end
end

module Record = struct
  type 't t = (string * 't) list

  let get k = List.find_opt (fst %> String.equal k) %> Option.map snd

  let get_exn k = Option.get_exn % get k
end

type t =
  | Record of t Record.t
  | Function of (t -> t)
  | Variant of string * (string * t) list
  | Primitive of Primitive.t

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
      const Primitive.pp p )
    ppf ()

and pp_record ppf =
  let open Fmt in
  ( any "{"
  ++ list ~sep:(any ",@ ") (pair ~sep:(any "@ =@ ") string pp)
  ++ any "}" )
    ppf

module Get = struct
  let record = function Record r -> Some r | _ -> None

  let function_ = function Function f -> Some f | _ -> None

  let variant = function Variant (v, r) -> Some (v, r) | _ -> None

  let primitive = function Primitive p -> Some p | _ -> None

  module Exn = struct
    let record = Option.get_exn % record

    let function_ = Option.get_exn % function_

    let variant = Option.get_exn % variant

    let primitive = Option.get_exn % primitive
  end
end
