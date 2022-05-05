open Sexplib

(* Det (x, A, b) === let x : A = b *)
type t =
  | Ident of string
  | Pi of { src: t; dst: binding }
  | Lam of binding
  | Ap of { f: t; a: t }
and binding = Binding of { names: string list; body: t }

type com = 
  | Def of string * t * t
  | Normalize of string

(* printing utilities *)

let rec cst_to_sexp t = 
  match t with
  | Ident s -> Sexp.(List [
      Atom "Ident";
      Atom s
    ])
  | Pi { src; dst = Binding { names; body } } -> 
    let names = Conv.sexp_of_list Conv.sexp_of_string names in
    Sexp.(List [
      Atom "Pi";
      cst_to_sexp src;
      List [names; cst_to_sexp body]
    ])
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

let cscom_to_sexp com = 
  match com with
  | Def (s, a, b) -> Sexp.(List [
      Atom "DEF";
      Atom s;
      cst_to_sexp a;
      cst_to_sexp b
    ])
  | Normalize s -> Sexp.(List [
      Atom "NORMALIZE";
      Atom s
    ])

let print_t t = t |> cst_to_sexp |> Sexp.to_string_hum |> print_endline