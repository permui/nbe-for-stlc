open Sexplib
module CS = Concrete_syntax

exception MyError of string

type t =
  | T (* the only base type *)
  | Var of int (* de bruijn index *)
  | Pi of { src: t; dst: t }
  | Lam of t
  | Ap of { f: t; a: t }


val remove_names: CS.t -> string list -> t

val syntax_t_to_sexp: t -> Sexp.t

val print_syntax_t: t -> unit

val to_concrete_syntax: t -> int * CS.t