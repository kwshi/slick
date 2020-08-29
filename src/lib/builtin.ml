open Containers
open Fun

let vals =
  let get_int_exn k =
    Val.Record.get_exn k
    %> Val.Get.Exn.primitive
    %> Val.Primitive.Get.Exn.int
  in
  let int_bop name f =
    name,
    Val.Function
      (fun args ->
         let args = Val.Get.Exn.record args in
         let a = get_int_exn "a" args in
         let b = get_int_exn "b" args in
         Primitive (Int (f a b))
      ),
    Type.Function
      (Type.Record
         (["a", Type.Primitive (Type.Int);
           "b", Type.Primitive (Type.Int)], None), Type.Primitive (Type.Int))
  in
  [ int_bop "int_add" Z.add ;
    int_bop "int_sub" Z.sub ;
    int_bop "int_mul" Z.mul ;
    int_bop "int_div" Z.div ;
  ]


let scope, ctx =
  vals
  |> List.map (fun (n, v, t) ->
      (n, v),
      Ctx.Var (n, t)
    )
  |> List.split
  |> Pair.map Scope.of_list Ctx.of_list
