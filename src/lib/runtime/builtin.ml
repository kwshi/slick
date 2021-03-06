open Containers
open Fun
open Slick_core

let vals =
  let get_int_exn = Value.Get.Exn.primitive %> Value.Primitive.Get.Exn.int in
  let bop in_t get out_t make name f =
    ( name
    , (let open Value in
      Function
        (fun a ->
          Function
            (fun b ->
              let a' = get a in
              let b' = get b in
              make (f a' b'))))
    , let open Type in
      Function (in_t, Function (in_t, out_t)) )
  in
  let int_bop =
    bop
      Type.(Primitive Int)
      (Value.Get.Exn.primitive %> Value.Primitive.Get.Exn.int)
      Type.(Primitive Int)
      (fun n -> Value.Primitive (Value.Primitive.Int n))
  in
  let int_comp =
    bop
      Type.(Primitive Int)
      (Value.Get.Exn.primitive %> Value.Primitive.Get.Exn.int)
      Type.bool
      Value.Make.bool
  in
  let bool_bop = bop Type.bool Value.Get.Exn.bool Type.bool Value.Make.bool in
  [ int_bop "+" Z.add
  ; int_bop "-" Z.sub
  ; int_bop "*" Z.mul
  ; int_bop "/" Z.div
  ; int_bop "%" Z.( mod )
  ; int_bop "**" (fun a p -> Z.pow a (Z.to_int p))
  ; int_comp "<=" Z.leq
  ; int_comp ">=" Z.geq
  ; int_comp "<" Z.lt
  ; int_comp ">" Z.gt
  ; int_comp "==" Z.equal
  ; int_comp "!=" (fun a b -> not @@ Z.equal a b)
  ; bool_bop "&&" ( && )
  ; bool_bop "||" ( || )
  ; ("true", Value.Make.bool true, Type.bool)
  ; ("false", Value.Make.bool false, Type.bool)
  ; ( "$-"
    , (let open Value in
      Function (fun v -> Primitive (Int (Z.neg @@ get_int_exn v))))
    , let open Type in
      Function (Primitive Int, Primitive Int) )
  ; ( "print"
    , (let open Value in
      Function
        ( Value.Get.Exn.primitive
        %> Value.Primitive.Get.Exn.str
        %> print_endline
        %> const Value.Make.unit ))
    , let open Type in
      Function (Primitive String, unit) )
  ; ( "++"
    , (let open Value in
      Function
        (fun a ->
          Function
            (fun b ->
              let a' = Get.Exn.primitive a |> Primitive.Get.Exn.str in
              let b' = Get.Exn.primitive b |> Primitive.Get.Exn.str in
              Primitive (String (a' ^ b')))))
    , let open Type in
      Function (Primitive String, Function (Primitive String, Primitive String))
    )
  ]


let scope, ctx =
  vals
  |> List.map (fun (n, v, t) -> ((n, v), Slick_core.Context.Var (n, t)))
  |> List.split
  |> Pair.map Scope.of_list Slick_core.Context.of_list
