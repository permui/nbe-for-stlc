open Lexing
open Mylib

let print_pos outx lexbuf = 
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d"
    pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let test_front =
  let inx = open_in "test/test.st" in
  let lexbuf = Lexing.from_channel inx in
  try 
    let coms = Mylib.Parser.prog Mylib.Lexer.read lexbuf in
    Driver.run coms
  with
  | Lexer.SyntaxError s ->
    Printf.eprintf "Lex Error: %a %s" print_pos lexbuf s
  | Parser.Error -> 
    Printf.eprintf "Parse Error: %a" print_pos lexbuf

let () = test_front

