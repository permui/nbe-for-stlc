open Sexplib
module CS = Concrete_syntax

exception Error of string

type t =
  | Var of int (* de bruijn index *)
  | Lam of t
  | Ap of { f: t; a: t }
and ty =
  | Base of int
  | Arrow of ty * ty


val remove_names_t: CS.t -> string list -> t
val remove_names_ty: CS.ty -> string list -> ty

val syntax_t_to_sexp: t -> Sexp.t

val print_syntax_t: t -> unit

val to_concrete_syntax: t -> int * CS.t