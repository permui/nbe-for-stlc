module S = Syntax

type env = t list
and clos = Clos of { term: S.t; env: env }
and t = 
  | T
  | Neutral of { term: ne; tp: t }
  | Pi of { src: t; dst: clos }
  | Lam of clos
and ne = 
  | Var of int (* de bruijn level *)
  | Ap of { f: ne; a: pack }
and pack = 
  | Pack of { term: t; tp: t }

let make_var size tp = 
  Neutral { term = Var size; tp }