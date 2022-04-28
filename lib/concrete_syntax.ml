

(* Let (x, A, b, c) === let x : A = b in c *)
type t =
  | Ident of string
  | Pi of { src: t; dst: binding }
  | Lam of binding
  | Ap of { f: t; a: t }
and binding = { names: string list; body: t }

type com = 
  | Def of string * t * t
  | Normalize of string