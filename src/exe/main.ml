
open Cmdliner


let file =
  let open Arg in
  pos 0 (some file) None @@
  info ~docv:"file" ~doc:"path to slick file" []
  |> value

let main =
  function
  | None -> 
    Repl.repl ()

  | Some s ->
    let ch = open_in s in
    Lexing.from_channel ch
    |> Slick.Parser.module_ Slick.Lexer.read
    |> List.fold_left
      (fun (m, sc, ctx) (s, e) ->
         let t, _ = Slick.Typing.infer_top ctx e in
         let v = Slick.Eval.evaluate sc e in
         Slick.Scope.add s (v, t) m
       , Slick.Scope.add s v sc
       , Slick.Context.(append_ctx [Var (s, t.tp)] ctx)
      )
      (Slick.Scope.empty, Slick.Builtin.scope, Slick.Builtin.ctx)
    |> ignore
  ; close_in ch
      


let () =
  let open Term in
  (const main $ file, info ~doc:"cool programming language" "slick")
  |> eval
  |> exit
