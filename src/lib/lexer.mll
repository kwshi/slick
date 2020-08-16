
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

let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let newline = "\r" | "\n" | "\r\n"

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ":" { COLON }
  | "=" { EQUALS }
  | "," { COMMA }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | _ { raise (Ast.SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) } 
  | eof { EOF }

