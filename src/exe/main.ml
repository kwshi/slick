open Cmdliner

let file =
  let open Arg in
  pos 0 (some file) None @@ info ~docv:"file" ~doc:"path to slick file" []
  |> value


let main = function
  | None ->
      Repl.repl ()
  | Some s ->
      let ch = open_in s in
      Lexing.from_channel ch
      |> Slick.Parser.module_ Slick.Lexer.read
      |> Slick.Module.eval
      |> ignore ;
      close_in ch


let () =
  let open Term in
  (const main $ file, info ~doc:"cool programming language" "slick")
  |> eval
  |> exit
