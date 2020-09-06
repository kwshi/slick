open Containers
module Map = Map.Make (String)

type t = Val.t Scope.t * Ctx.t

let eval =
  List.fold_left
    (fun (sc, ctx) (s, e) ->
      let t, _ = Typing.infer_top ctx e in
      let v = Eval.evaluate sc e in
      (Scope.add s v sc, Ctx.(append_ctx [ Var (s, t.tp) ] ctx)))
    (Builtin.scope, Builtin.ctx)
