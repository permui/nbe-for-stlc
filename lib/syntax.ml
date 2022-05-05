open Utils
open Sexplib
module CS = Concrete_syntax

exception Error of string

type t =
  | Annot of t * ty
  | Var of int (* de bruijn index *)
  | Lam of t
  | Ap of { f: t; a: t }
and ty =
  | Base of int
  | Arrow of ty * ty


let rec remove_names_t t names bases =
  match t with
  | CS.Annot (tm, ty) ->
    let tm' = remove_names_t tm names bases in
    let ty' = remove_names_ty ty bases in
    Annot (tm', ty')
  | CS.TermIdent s -> 
    begin 
      match get_index names s with
      | None -> raise @@ Error ("name `" ^ s ^ "` unbound")
      | Some k -> Var k;
    end
  | CS.Lam (CS.Binding { names = ns; body }) ->
    let term = remove_names_t body (ns @ names) bases in
    Lam term
  | CS.Ap { f; a } ->
    let f = remove_names_t f names bases in
    let a = remove_names_t a names bases in
    Ap { f; a }

and remove_names_ty ty bases = 
  match ty with
  | CS.TypeIdent s ->
    begin
      match get_index bases s with
      | None -> raise @@ Error ("basetype name `" ^ s ^ "` not found")
      | Some k -> Base k
    end
  | CS.Arrow (t1, t2) ->
    let t1' = remove_names_ty t1 bases in
    let t2' = remove_names_ty t2 bases in
    Arrow (t1', t2')


(* printing utilities *)

let rec syntax_t_to_sexp t =
  match t with
  | Annot (t, ty) -> Sexp.(List [Atom "S.Annot"; syntax_t_to_sexp t; syntax_ty_to_sexp ty])
  | Var i -> Sexp.(List [Atom "S.Var"; Atom (string_of_int i)])
  | Lam t -> Sexp.(List [Atom "S.Lam"; syntax_t_to_sexp t])
  | Ap { f; a } -> Sexp.(List [
      Atom "S.Ap";
      syntax_t_to_sexp f;
      syntax_t_to_sexp a 
    ])
and syntax_ty_to_sexp ty = 
  let rec aux ty = 
    match ty with
    | Base k -> "Base(" ^ (string_of_int k) ^ ")"
    | Arrow (t1, t2) -> Printf.sprintf "(%s -> %s)" (aux t1) (aux t2)
  in 
  Sexp.Atom (aux ty)

let print_syntax_t t = t |> syntax_t_to_sexp |> Sexp.to_string_hum |> print_endline

let t_to_concrete_syntax t = 
  let new_varname k = "x" ^ (string_of_int k) in 
  let rec aux t cnt var_stack =
    match t with
    | Var k -> (cnt, CS.TermIdent (List.nth var_stack k))
    | Lam t ->
      let var_name = new_varname cnt in 
      let (cnt, ct) = aux t (cnt + 1) (var_name :: var_stack) in
      (cnt, CS.Lam (CS.Binding { names = [var_name]; body = ct }))
    | Ap { f = t1; a = t2 } ->
      let (cnt, t1') = aux t1 cnt var_stack in 
      let (cnt, t2') = aux t2 cnt var_stack in 
      (cnt, CS.Ap { f = t1'; a = t2' })
    | _ -> raise @@ Error ("this should not get to concrete syntax")
  in
  aux t 0 []