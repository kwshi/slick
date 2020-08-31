open Containers
open Fun

let vals =
  let get_int_exn =
    Val.Get.Exn.primitive %> Val.Primitive.Get.Exn.int
  in
  let int_bop name f =
    ( name
    , Val.Function
        (fun a ->
           Val.Function
             (fun b ->
                let a' = get_int_exn a in
                let b' = get_int_exn b in
                Val.(Primitive (Primitive.Int (f a' b')))
             )
        )
    ,
    let open Type in
    Function
        ( Primitive Int , Function (Primitive Int, Primitive Int) ) )
  in
  let int_comp name f =
    (name,
     Val.Function
       (fun a ->
          Val.Function
            (fun b ->
               let a' = get_int_exn a in
               let b' = get_int_exn b in
               Variant ((if f a' b' then "True" else "False"), Record [])
            )
       ),
     let open Type in
     Function
       (Primitive Int, Function (Primitive Int, bool))
    )
  in
  [ int_bop "+" Z.add
  ; int_bop "-" Z.sub
  ; int_bop "*" Z.mul
  ; int_bop "/" Z.div
  ; int_comp "<=" Z.leq
  ; int_comp ">=" Z.geq
  ; int_comp "<" Z.lt
  ; int_comp ">" Z.gt
  ; int_comp "==" Z.equal
  ; int_comp "!=" (fun a b -> not @@ Z.equal a b)
  ; "$-"
  , (let open Val in
     Function
       (fun v -> Primitive (Int (Z.neg @@ get_int_exn v)))
    )
  , (let open Type in
     Function (Primitive Int, Primitive Int)
    )
  ; "print"
  , (let open Val in
       Function
         (Val.Get.Exn.primitive 
          %> Val.Primitive.Get.Exn.str
          %> print_endline
          %> const (Val.Record [])))
     , (let open Type in
        Function (Primitive String, unit))
  ]

let scope, ctx =
  vals
  |> List.map (fun (n, v, t) -> ((n, v), Ctx.Var (n, t)))
  |> List.split
  |> Pair.map Scope.of_list Ctx.of_list
