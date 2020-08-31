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

let pp =
  let open Fmt in
  vbox (styled `Bold @@ styled (`Fg (`Hi `Blue)) @@ pp_logo_lines)
  ++ hbox
       ( sp
       ++ styled `Faint
         (styled `Italic @@
            const string @@ Random.run (Random.pick_array splash_phrases)) )
  ++ Format.pp_force_newline ++ Format.pp_print_newline


