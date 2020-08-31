
{
open Parser
open Lexing

let next_line lb =
  let pos = lb.lex_curr_p in
  lb.lex_curr_p <- { pos with
    pos_bol = lb.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1;
  }

(* let convert_string str =
 *   let open Seq in
 *   let rec go x = match x () with
 *   | Cons ('"', Nil) -> Nil
 *   | Cons ('\\', rest) ->
 *     (match rest () with
 *     | Cons ('"', rest) -> Cons ('"', go rest)
 *     | Cons ('\\', rest) -> Cons ('\\', go rest)
 *     | Cons ('n', rest) -> Cons ('\n', go rest)
 *     | Cons ('r', rest) -> Cons ('\r', go rest)
 *     | Cons ('t', rest) -> Cons ('\t', go rest)
 *     | _ -> failwith "lexing error: malformed string literal."
 *     )
 *   | Cons (c, rest) -> Cons (c, go rest)
 *   | Nil -> failwith "lexing error: string not terminated by quotation (this shouldn't happen)."
 *   in match List.of_seq @@ String.to_seq str with
 *      | Cons ('"', rest) -> String.of_seq (go rest)
 *      | _ -> failwith "lexing error: string not beginning with quotation (this shouldn't happen)." *)
}


let lower_ident = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* | '_'

let upper_ident = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let nonnegative_digits = '0' | ['1'-'9'] ['0'-'9']*

let newline = "\r" | "\n" | "\r\n"

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | "case" { CASE }
  | "\\" { BACKSLASH }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ":" { COLON }
  | "|" { PIPE }
  | ";" { SEMICOLON }
  | "=" { EQUALS }
  | ":=" { WALRUS }
  | ',' { COMMA }
  | '.' { DOT }
  | '-' { MINUS }
  | '+' { PLUS }
  | '*' { ASTERISK }
  | '/' { SLASH }
  | '"' { STRING (read_string (Buffer.create 16) lexbuf) }
  | nonnegative_digits { INT (Lexing.lexeme lexbuf |> Z.of_string) } 
  | lower_ident { LOWER_IDENT (Lexing.lexeme lexbuf) }
  | upper_ident { UPPER_IDENT (Lexing.lexeme lexbuf) }
  | _ { raise (Ast.SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) } 
  | eof { EOF }

and read_string buf =
  parse
  | "\\n" { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | "\\t" { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | "\\\"" { Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | '"' { Buffer.contents buf }
