module S = Syntax

type env = t list
and clos = Clos of { term: S.t; env: env }
and t = 
  | Neutral of { term: ne; tp: ty }
  | Lam of clos
and ty =
  | Base of int
  | Arrow of ty * ty
and ne = 
  | Var of int (* de bruijn level *)
  | Ap of { f: ne; a: pack }
and pack = 
  | Pack of { term: t; tp: ty }

let make_var size tp = 
  Neutral { term = Var size; tp }