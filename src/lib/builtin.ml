open Containers
open Fun

let vals =
  let get_int_exn k =
    Val.Record.get_exn k
    %> Val.Get.Exn.primitive
    %> Val.Primitive.Get.Exn.int
  in
  [ "int_add",
    Val.Function
      (fun args ->
         let args = Val.Get.Exn.record args in
         let a = get_int_exn "a" args in
         let b = get_int_exn "b" args in
         Primitive (Int (Z.add a b))
      ),
    Type.Function
      (Type.Record
         (["a", Type.Primitive (Type.Int);
           "b", Type.Primitive (Type.Int)], None), Type.Primitive (Type.Int))
  ]


let scope, ctx =
  vals
  |> List.map (fun (n, v, t) ->
      (n, v),
      Ctx.Var (n, t)
    )
  |> List.split
  |> Pair.map Scope.of_list Ctx.of_list
