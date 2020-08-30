open Containers
open Fun

let pp_logo_lines ppf () =
  let open Fmt in
  let h = styled (`Fg (`Hi `Green)) % any in
  let h' = styled `None % styled (`Fg `Green) % any in
  let a = any in
  [ sps 4 ++ h "__" ++ sps 7 ++ a "__"
  ; sp ++ a "___" ++ h "\\ \\" ++ a "__  __\\ \\  __"
  ; a "/ ___" ++ h "\\ \\" ++ a " \\/ __\\ \\/ /"
  ; a "\\____ " ++ h "\\ \\" ++ a " \\ \\__\\   \\_"
  ; a "____" ++ h' "/" ++ h "_/\\_\\" ++ a "_\\___/\\_\\___" ]
  |> List.map (hbox % ( ++ ) (sps 2))
  |> List.intersperse cut
  |> List.iter (fun pp -> pp ppf ())

let pp_logo =
  let open Fmt in
  vbox (styled `Bold @@ styled (`Fg (`Hi `Blue)) @@ pp_logo_lines)
  ++ hbox (sp ++ styled `Faint (any "insert catchphrase here"))
  ++ Format.pp_force_newline ++ Format.pp_print_newline

let pp =
  let open Fmt in
  hvbox
    (pair
       ~sep:(sp ++ styled `Faint (hbox (any ":@ ")))
       (styled (`Fg (`Hi `Cyan)) Slick.Value.pp)
       (styled `Faint Slick.Type.pp))
  ++ const Format.pp_print_newline ()

let () =
  Fmt.(set_style_renderer stdout `Ansi_tty) ;
  Fmt.(set_style_renderer stderr `Ansi_tty) ;
  LNoise.catch_break true ;
  pp_logo Fmt.stderr () ;
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
          stderr ()
    | Some s -> (
      match String.trim s with
      | "" ->
          ()
      | input ->
          LNoise.history_add input |> ignore ;
          let expr, _ =
            Lexing.from_string input
            |> Slick.Parser.prog Slick.Lexer.read
            |> Slick.Typing.infer_top Slick.Builtin.ctx
          in
          pp Fmt.stdout
            ( Slick.Eval.evaluate Slick.Builtin.scope expr
            , expr.Slick.Ast.Expr.tp ) )
  done
