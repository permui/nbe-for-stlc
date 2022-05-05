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

val cst_to_sexp: t -> Sexplib.Sexp.t

val cscom_to_sexp: com -> Sexplib.Sexp.t
val print_t: t -> unit