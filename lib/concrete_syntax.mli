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
val cst_to_sexp: t -> Sexplib.Sexp.t

val cscom_to_sexp: com -> Sexplib.Sexp.t
val print_t: t -> unit