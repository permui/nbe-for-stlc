open Utils
open Sexplib
module CS = Concrete_syntax

exception MyError of string

type t =
  | T (* the only base type *)
  | Var of int (* de bruijn index *)
  | Pi of { src: t; dst: t }
  | Lam of t
  | Ap of { f: t; a: t }


let rec remove_names t names =
  match t with
  | CS.Ident "T" -> T
  | CS.Ident s -> 
    begin 
      match get_index names s with
      | None -> raise @@ MyError ("name `" ^ s ^ "` unbound")
      | Some k -> Var k;
    end
  | CS.Pi { src; dst = CS.Binding { names = ns; body } } ->
    let src = remove_names src names in
    let dst = remove_names body (ns @ names) in
    Pi { src; dst }
  | CS.Lam (CS.Binding { names = ns; body }) ->
    let term = remove_names body (ns @ names) in
    Lam term
  | CS.Ap { f; a } ->
    let f = remove_names f names in
    let a = remove_names a names in
    Ap { f; a }


(* printing utilities *)

let rec syntax_t_to_sexp t =
  match t with
  | T -> Sexp.Atom "T"
  | Var i -> Sexp.(List [Atom "S.Var"; Atom (string_of_int i)])
  | Pi { src; dst } -> Sexp.(List [
      Atom "S.Pi";
      syntax_t_to_sexp src;
      syntax_t_to_sexp dst
    ])
  | Lam t -> Sexp.(List [Atom "S.Lam"; syntax_t_to_sexp t])
  | Ap { f; a } -> Sexp.(List [
      Atom "S.Ap";
      syntax_t_to_sexp f;
      syntax_t_to_sexp a 
    ])

let print_syntax_t t = t |> syntax_t_to_sexp |> Sexp.to_string_hum |> print_endline

let to_concrete_syntax t = 
  let new_varname k = "x" ^ (string_of_int k) in 
  let rec aux t cnt var_stack =
    match t with
    | T -> (cnt, CS.Ident "T")
    | Var k -> (cnt, CS.Ident (List.nth var_stack k))
    | Pi { src; dst } -> 
      let (cnt, src') = aux src cnt var_stack in
      let var_name = new_varname cnt in 
      let (cnt, body') = aux dst (cnt + 1) (var_name :: var_stack) in
      (cnt, CS.Pi { src = src'; dst = CS.Binding { names = [var_name]; body = body' } })
    | Lam t ->
      let var_name = new_varname cnt in 
      let (cnt, ct) = aux t (cnt + 1) (var_name :: var_stack) in
      (cnt, CS.Lam (CS.Binding { names = [var_name]; body = ct }))
    | Ap { f = t1; a = t2 } ->
      let (cnt, t1') = aux t1 cnt var_stack in 
      let (cnt, t2') = aux t2 cnt var_stack in 
      (cnt, CS.Ap { f = t1'; a = t2' })
  in
  aux t 0 []
  