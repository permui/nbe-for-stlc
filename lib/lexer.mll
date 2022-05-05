{
open Parser

exception SyntaxError of string
}

let id_start = ['A'-'Z' 'a'-'z' '_']
let id_more  = ['A'-'Z' 'a'-'z' '_' '0'-'9']
let id = id_start id_more*
let white = [' ' '\t']+
let newline = '\n'

rule read = 
  parse
  | white { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | "->" { ARROW }
  | '(' { LPAR }
  | ')' { RPAR }
  | ':' { COLON }
  | "fun" { FUN }
  | "basetype" { BASETYPE }
  | "def" { DEF }
  | "normalize" { NORMALIZE }
  | "::" { ANNOT }
  | '=' { EQ }
  | id { ID (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("unexpected char " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }