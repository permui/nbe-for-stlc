module S = Syntax
module D = Domain

exception TypeError of string

val check: S.t -> D.ty -> D.ty list -> unit