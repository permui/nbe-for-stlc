open Sexplib
module CS = Concrete_syntax

let rec cst_to_sexp t = 
  match t with
  | CS.Ident s -> Sexp.(List [
      Atom "Ident";
      Atom s
    ])
  | CS.Pi { src; dst } -> 
    let names = Conv.sexp_of_list Conv.sexp_of_string dst.names in
    Sexp.(List [
      Atom "Pi";
      cst_to_sexp src;
      List [names; cst_to_sexp dst.body]
    ])
  | CS.Lam b -> 
    let names = Conv.sexp_of_list Conv.sexp_of_string b.names in
    Sexp.(List [
      Atom "Lam";
      names;
      cst_to_sexp b.body
    ])
  | CS.Ap { f; a } -> Sexp.(List [
      Atom "Ap";
      cst_to_sexp f;
      cst_to_sexp a
    ])

let cscom_to_sexp com = 
  match com with
  | CS.Def (s, a, b) -> Sexp.(List [
      Atom "DEF";
      Atom s;
      cst_to_sexp a;
      cst_to_sexp b
    ])
  | CS.Normalize s -> Sexp.(List [
      Atom "NORMALIZE";
      Atom s
    ])