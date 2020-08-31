
{
open Parser
open Lexing

let next_line lb =
  let pos = lb.lex_curr_p in
  lb.lex_curr_p <- { pos with
    pos_bol = lb.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1;
  }
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
  | "def" { DEF }
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
  | "," { COMMA }
  | "." { DOT }
  | "-" { HYPHEN }
  | nonnegative_digits { INT (Lexing.lexeme lexbuf |> Z.of_string) } 
  | lower_ident { LOWER_IDENT (Lexing.lexeme lexbuf) }
  | upper_ident { UPPER_IDENT (Lexing.lexeme lexbuf) }
  | _ { raise (Ast.SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) } 
  | eof { EOF }

