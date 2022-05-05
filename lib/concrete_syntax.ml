open Sexplib

(* Det (x, A, b) === let x : A = b *)
type t =
  | TermIdent of string
  | Lam of binding
  | Ap of { f: t; a: t }
and ty = 
  | TypeIdent of string
  | Arrow of ty * ty
and binding = Binding of { names: string list; body: t }

type com = 
  | Basetype of string
  | Def of string * ty * t
  | Normalize of string

(* printing utilities *)

let rec cst_to_sexp t = 
  match t with
  | TermIdent s -> Sexp.Atom s
  | Lam (Binding { names; body }) -> 
    let names = Conv.sexp_of_list Conv.sexp_of_string names in
    Sexp.(List [
      Atom "Lam";
      names;
      cst_to_sexp body
    ])
  | Ap { f; a } -> Sexp.(List [
      Atom "Ap";
      cst_to_sexp f;
      cst_to_sexp a
    ])

let csty_to_sexp ty = 
  let rec csty_to_string ty = 
    match ty with
    | TypeIdent s -> s
    | Arrow (t1, t2) -> 
      let s1 = csty_to_string t1 in
      let s2 = csty_to_string t2 in
      Printf.sprintf "(%s -> %s)" s1 s2
  in
  Sexp.Atom (csty_to_string ty)

let cscom_to_sexp com = 
  match com with
  | Basetype s -> Sexp.(List [
      Atom "BASETYPE";
      Atom s
    ])
  | Def (s, a, b) -> Sexp.(List [
      Atom "DEF";
      Atom s;
      csty_to_sexp a;
      cst_to_sexp b
    ])
  | Normalize s -> Sexp.(List [
      Atom "NORMALIZE";
      Atom s
    ])

let print_t t = t |> cst_to_sexp |> Sexp.to_string_hum |> print_endline