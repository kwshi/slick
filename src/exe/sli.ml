open Containers

let pp =
  let open Fmt in
  hvbox
    (pair
       ~sep:(sp ++ styled (`Fg `Blue) (hbox (any ":@ ")))
       (styled (`Fg (`Hi `Cyan)) Slick.Value.pp)
       (styled (`Fg (`Hi `Blue)) @@ Slick.Type.pp))
  ++ const Format.pp_print_newline ()


let () =
  Fmt.(set_style_renderer stdout `Ansi_tty) ;
  Fmt.(set_style_renderer stderr `Ansi_tty) ;
  while true do
    match LNoise.linenoise "slick> " with
    | None ->
        let open Fmt in
        styled (`Fg (`Hi `Magenta)) (any "[exiting]") stderr () ;
        exit 0
    | exception Sys.Break ->
        let open Fmt in
        ( styled (`Fg (`Hi `Magenta)) (any "[interrupted]")
        ++ Format.pp_print_newline )
          stderr
          ()
    | Some s ->
      ( match String.trim s with
      | "" ->
          ()
      | input ->
          LNoise.history_add input |> Result.get_exn ;
          let expr, _ =
            Lexing.from_string input
            |> Slick.Parser.prog Slick.Lexer.read
            |> Slick.Typing.infer_top Slick.Builtin.ctx
          in
          pp
            Fmt.stdout
            ( Slick.Eval.evaluate Slick.Builtin.scope expr
            , expr.Slick.Ast.Expr.tp ) )
  done
