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
  ; a "\\____" ++ h' "/" ++ h "\\ \\" ++ a " \\ \\__\\   \\_"
  ; a "____" ++ h' "/_/" ++ h "\\_\\" ++ a "_\\___/\\_\\___" ]
  |> List.map (hbox % ( ++ ) (sps 2))
  |> List.intersperse cut
  |> List.iter (fun pp -> pp ppf ())

let splash_phrases =
  [| "insert catchphrase here"
   ; "yum yum"
   ; "slick (noun). A neat programming language"
   ; "Slick Lang Is Cool, K?"
   ; "slick ties your shoes for you"
   ; "hi."
   ; "Langy McLangface"
   ; "if slick were a color, it would be slate"
   ; "the slicki-wicki-wickest there is"
   ; "powered by types (TM)" |]

let pp_logo =
  let open Fmt in
  vbox (styled `Bold @@ styled (`Fg (`Hi `Blue)) @@ pp_logo_lines)
  ++ hbox
       ( sp
       ++ styled `Faint
         (styled `Italic @@
            const string @@ Random.run (Random.pick_array splash_phrases)) )
  ++ Format.pp_force_newline ++ Format.pp_print_newline

let pp =
  let open Fmt in
  hbox
    (pair
       ~sep:(sp ++ styled `Faint (hbox (any ":@ ")))
       (styled (`Fg (`Hi `Cyan)) Slick.Value.pp)
       (styled `Faint Slick.Type.pp))
  ++ const Format.pp_print_newline ()

let err pp = 
  let open Fmt in
  (styled (`Fg (`Hi `Red)) (hovbox pp)
   ++ Format.pp_print_newline
  ) stderr ()
; exit 1

let eval sc ctx e =
  (match Slick.Typing.infer ctx e with
   | exception Failure s ->
     err @@ Fmt.(const string s)
              
   | expr, _ ->
     ( Slick.Eval.evaluate sc expr
     , expr.Slick.Ast.Expr.tp )
  )

let rec repl sc ctx =
  match LNoise.linenoise "slick> " with
  | None ->
    let open Fmt in
    styled (`Fg (`Hi `Magenta)) (any "[exiting]") stderr ()

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
        match Lexing.from_string input
              |> Slick.Parser.repl Slick.Lexer.read
        with
        | exception Slick.Ast.SyntaxError s ->
          err @@ Fmt.(styled `Bold (any "syntax@ error:@ ") ++ const string s)
                   
        | exception Slick.Parser.Error ->
          err @@ Fmt.(styled `Bold @@ any "syntax error")
                   
        | Slick.Ast.Repl.Expr e ->
          pp Fmt.stdout @@ eval sc ctx e
        ; repl sc ctx
            
        | Slick.Ast.Repl.Def (s, e) ->
          let v, t = eval sc ctx e in
          pp Fmt.stdout @@ (v, t)
        ; repl
            (Slick.Scope.add s v sc)
            (Slick.Context.append_ctx [Slick.Context.Var (s, t)] ctx)
    )
    

let () =
  Fmt.(set_style_renderer stdout `Ansi_tty) ;
  Fmt.(set_style_renderer stderr `Ansi_tty) ;
  LNoise.catch_break true ;
  LNoise.set_multiline true ;
  pp_logo Fmt.stderr () ;
  repl Slick.Builtin.scope Slick.Builtin.ctx
    
