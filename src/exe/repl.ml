open Containers
open Fun

let pp =
  let open Fmt in
  hbox
    (pair
       ~sep:(sp ++ styled `Faint (hbox (any ":@ ")))
       (styled (`Fg (`Hi `Cyan)) Slick_core.Value.pp)
       (styled `Faint Slick_core.Type.pp))
  ++ const Format.pp_print_newline ()


let rec repl (sc, ctx) : unit =
  let err pp =
    let open Fmt in
    (styled (`Fg (`Hi `Red)) (hovbox pp) ++ Format.pp_print_newline) stderr () ;
    repl (sc, ctx)
  in
  let eval e : Slick_core.Value.t * Slick_core.Type.t =
    let expr, _ = Slick_core.Typing.infer_top ctx e in
    (Slick_core.Eval.evaluate sc expr, expr.Slick_ast.Expr.tp)
  in
  let eval_def s e : Slick_core.Value.t * Slick_core.Type.t =
    let expr, _ = Slick_core.Typing.infer_def ctx s e in
    (Slick_core.Eval.evaluate sc expr, expr.Slick_ast.Expr.tp)
  in
  match LNoise.linenoise "slick> " with
  | None ->
      let open Fmt in
      styled (`Fg (`Hi `Magenta)) (any "[exiting]") stderr ()
  | exception Sys.Break ->
      let open Fmt in
      ( styled (`Fg (`Hi `Magenta)) (any "[interrupted]")
      ++ Format.pp_print_newline )
        stderr
        () ;
      repl (sc, ctx)
  | Some input ->
      LNoise.history_add input |> ignore ;
      ( try
          Lexing.from_string input
          |> Slick_parser.Parser.repl Slick_parser.Lexer.read
          |> function
          | Slick_ast.Repl.Empty ->
              repl (sc, ctx)
          | Slick_ast.Repl.Expr e ->
              pp Fmt.stdout @@ eval e ;
              repl (sc, ctx)
          | Slick_ast.Repl.Def (s, e) ->
              let v, t = eval_def s e in
              pp Fmt.stdout @@ (v, t) ;
              repl
                ( Slick_core.Scope.add s v sc
                , Slick_core.Context.append_ctx [ Slick_core.Context.Var (s, t) ] ctx )
          | Slick_ast.Repl.Cmd ("load", f) ->
              if Sys.file_exists f
              then (
                let ch = open_in f in
                let sc', ctx' =
                  Lexing.from_channel ch
                  |> Slick_parser.Parser.module_ Slick_parser.Lexer.read
                  |> Slick_core.Module.eval (sc, ctx)
                in
                close_in ch ;
                repl
                  ( Slick_core.Scope.union (fun _ _ b -> Some b) sc sc'
                  , Slick_core.Context.append_ctx ctx'.context ctx ) )
              else
                err
                @@ Fmt.(
                     any "file@ `"
                     ++ const string f
                     ++ any "`@ does@ not@ exist!")
          | Slick_ast.Repl.Cmd (c, _) ->
              err
              @@ Fmt.(
                   styled `Bold (any "invalid@ command:@ ") ++ const string c)
        with
      | Slick_ast.SyntaxError s ->
          err @@ Fmt.(styled `Bold (any "syntax@ error:@ ") ++ const string s)
      | Slick_parser.Parser.Error ->
          err @@ Fmt.(styled `Bold @@ any "syntax error")
      | Failure s ->
          err @@ Fmt.(const string s) )


let repl () =
  Fmt.(set_style_renderer stdout `Ansi_tty) ;
  Fmt.(set_style_renderer stderr `Ansi_tty) ;
  LNoise.catch_break true ;
  LNoise.set_multiline true ;
  Logo.pp Fmt.stderr () ;
  repl Slick_runtime.Prelude.prelude
