module S = Syntax
module D = Domain

exception NbeError of string

val eval_t: S.t -> D.env -> D.t
val eval_ty: S.ty -> D.ty

val read_back: int -> D.pack -> S.t