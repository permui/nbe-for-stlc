open Cmdliner
open Lexing
open Mylib

let print_pos outx lexbuf = 
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s : line %d : position %d"
    pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let process path =
  try 
    let inx = open_in path in
    let lexbuf = Lexing.from_channel inx in
    set_filename lexbuf path; 
    try 
      let coms = Mylib.Parser.prog Mylib.Lexer.read lexbuf in
      Driver.run coms
    with
    | Lexer.SyntaxError s ->
      Printf.eprintf "Lex Error: %a %s\n" print_pos lexbuf s
    | Parser.Error -> 
      Printf.eprintf "Parse Error: %a\n" print_pos lexbuf 
  with
  | Sys_error s -> Printf.eprintf "Sys Error: %s\n" s

let path = 
  let doc = "The STLC program to be processed." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")

let process_t = Term.(const process $ path)

let cmd = 
  let doc = "process an STLC program" in
  let info = Cmd.info "stlc" ~doc in
  Cmd.v info process_t

let main () = exit (Cmd.eval cmd)

let () = main ()