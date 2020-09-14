open Containers
open Fun

module Primitive = struct
  type t =
    | Int of Z.t
    | String of string

  let pp_string_esc =
    let open Fmt in
    let dim = styled `Faint % styled (`Fg `Cyan) in
    let esc = styled (`Fg `Yellow) % any in
    fun ppf s ->
      dim (any "“") ppf () ;
      String.iter
        (fun c ->
          ( match c with
          | '\n' ->
              esc "\\n"
          | '\t' ->
              esc "\\t"
          | '\r' ->
              esc "\\r"
          | '"' ->
              esc "\\\""
          | '\\' ->
              esc "\\\\"
          | _ ->
              const char c )
            ppf
            ())
        s ;
      dim (any "”") ppf ()


  let pp ppf p =
    let open Fmt in
    ( match p with
    | Int n ->
        const Z.pp_print n
    | String s ->
        const pp_string_esc s )
      ppf
      ()


  module Get = struct
    let int = function Int n -> Some n | _ -> None

    let str = function String s -> Some s | _ -> None

    module Exn = struct
      let int = Option.get_exn % int

      let str = Option.get_exn % str
    end
  end
end

module Record = struct
  type 't t = (string * 't) list

  let get k = List.find_opt (fst %> String.equal k) %> Option.map snd

  let get_exn k = Option.get_exn % get k
end

module Tuple = struct
  type 't t =
    { unlabeled : 't list
    ; labeled : (string * 't) list
    }

  let index n t = List.nth_opt t.unlabeled n
  let get k t = List.find_opt (fst %> String.equal k) t.labeled |> Option.map snd

  module Exn = struct
    let index n = Option.get_exn % index n
    let get k = Option.get_exn % get k
  end
end

type t =
  | Record of t Record.t (* deprecate *)
  | Tuple of t Tuple.t
  | Function of (t -> t)
  | Variant of string * t
  | Primitive of Primitive.t

let rec pp ppf value =
  let open Fmt in
  ( match value with
  | Record r ->
      const pp_record r
  | Tuple t ->
    const pp_tuple t
  | Function _ ->
      any "<function>"
  | Variant (v, e) ->
      const string v ++ (match e with Record [] -> nop | _ -> sp ++ const pp e)
  | Primitive p ->
      const Primitive.pp p )
    ppf
    ()


and pp_record ppf =
  let open Fmt in
  ( any "{"
  ++ list ~sep:(any ",@ ") (pair ~sep:(any "@ =@ ") string pp)
  ++ any "}" )
    ppf

and pp_tuple ppf t =
  let open Fmt in
  let ppu = list ~sep:comma pp in
  let ppl = list ~sep:comma (pair ~sep:(any "=") string pp) in
  (any "("
  ++ (match t.Tuple.unlabeled, t.labeled with
       | [], [] ->
         nop
       | [v], [] ->
         const pp v ++ any ","
       | [], l ->
         const ppl l
       | u, [] ->
         const ppu u
       | u, l ->
         const (pair ~sep:comma ppu ppl) (u, l)
     )
  ++ any ")"
 ) ppf ()
  


module Make = struct
  let unit = Record []

  let bool b = Variant ((if b then "True" else "False"), unit)
end

module Get = struct
  let record = function Record r -> Some r | _ -> None

  let function_ = function Function f -> Some f | _ -> None

  let variant = function Variant (v, r) -> Some (v, r) | _ -> None

  let primitive = function Primitive p -> Some p | _ -> None

  let bool = function
    | Variant ("True", Record []) ->
        Some true
    | Variant ("False", Record []) ->
        Some false
    | _ ->
        None


  module Exn = struct
    let record = Option.get_exn % record

    let function_ = Option.get_exn % function_

    let variant = Option.get_exn % variant

    let primitive = Option.get_exn % primitive

    let bool = Option.get_exn % bool
  end
end

let rec match_pat =
  let module Pat = Slick_ast.Pattern in
  function
  | Pat.Var var, v ->
      Some [ (var, v) ]
  | Pat.Record fields, Record r ->
      List.fold_right
        (fun (lbl, pat') accum_opt ->
          match (accum_opt, Record.get lbl r) with
          | Some accum, Some v ->
            ( match match_pat (pat', v) with
            | Some bindings ->
                Some (bindings @ accum)
            | None ->
                None )
          | _ ->
              None)
        fields
        (Some [])
  | Pat.Variant (lbl, pat'), Variant (lbl', v)
    when String.(equal lbl lbl') ->
      match_pat (pat', v)
  | Pat.Literal (Int i), Primitive (Primitive.Int i')
    when Z.equal i i' ->
      Some []
  | Pat.Literal (String s), Primitive (Primitive.String s')
    when String.(equal s s') ->
      Some []
  | _ ->
      None
