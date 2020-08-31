open Containers
open Fun
    
let pp =
  let open Fmt in
  hbox
    (pair
       ~sep:(sp ++ styled `Faint (hbox (any ":@ ")))
       (styled (`Fg (`Hi `Cyan)) Slick.Value.pp)
       (styled `Faint Slick.Type.pp))
  ++ const Format.pp_print_newline ()


let rec repl sc ctx : unit =
  let err pp = 
    let open Fmt in
    (styled (`Fg (`Hi `Red)) (hovbox pp)
     ++ Format.pp_print_newline
    ) stderr ()
  ; repl sc ctx 
  in
  let eval e : Slick.Value.t * Slick.Type.t =
    let expr, _ = Slick.Typing.infer_top ctx e in
    ( Slick.Eval.evaluate sc expr
    , expr.Slick.Ast.Expr.tp )
  in
  match LNoise.linenoise "slick> " with
  | None ->
    let open Fmt in
    styled (`Fg (`Hi `Magenta)) (any "[exiting]") stderr ()

  | exception Sys.Break ->
    let open Fmt in
    ( styled (`Fg (`Hi `Magenta)) (any "[interrupted]")
      ++ Format.pp_print_newline )
      stderr ()
  ; repl sc ctx

  | Some input -> (
      LNoise.history_add input |> ignore ;
      try
        Lexing.from_string input
        |> Slick.Parser.repl Slick.Lexer.read 
        |> function
        | Slick.Ast.Repl.Empty ->
          repl sc ctx
            
        | Slick.Ast.Repl.Expr e ->
          pp Fmt.stdout @@ eval e
        ; repl sc ctx
            
        | Slick.Ast.Repl.Def (s, e) ->
          let v, t = eval e in
          pp Fmt.stdout @@ (v, t)
        ; repl
            (Slick.Scope.add s v sc)
            (Slick.Context.append_ctx [Slick.Context.Var (s, t)] ctx)
            
      with
      | Slick.Ast.SyntaxError s ->
        err @@ Fmt.(styled `Bold (any "syntax@ error:@ ") ++ const string s)
                 
      | Slick.Parser.Error ->
        err @@ Fmt.(styled `Bold @@ any "syntax error")
                 
      | Failure s ->
        err @@ Fmt.(const string s)
    )
    
let repl () =
  Fmt.(set_style_renderer stdout `Ansi_tty) ;
  Fmt.(set_style_renderer stderr `Ansi_tty) ;
  LNoise.catch_break true ;
  LNoise.set_multiline true ;
  Logo.pp Fmt.stderr () ;
  repl Slick.Builtin.scope Slick.Builtin.ctx
    
