module S = Syntax
module D = Domain

exception NbeError of string

val eval: S.t -> D.env -> D.t

val read_back: int -> D.pack -> S.t