module S = Syntax
module D = Domain

exception NbeError of string

let rec do_clos (D.Clos { term; env }) a = eval term (a :: env)

and do_ap f a =
  match f with
  | D.Lam clos -> do_clos clos a
  | D.Neutral { term; tp = D.Pi { src; dst } } ->
    let tp = do_clos dst a in
    D.Neutral { term = D.Ap { f = term; a = Pack { term = a; tp = src } }; tp = tp }
  | _ -> raise @@ NbeError ("not a function in do_ap")

and eval t env = 
  match t with
  | S.T -> D.T
  | S.Var k -> List.nth env k
  | S.Pi { src; dst } -> 
    let src' = eval src env in 
    D.Pi { src = src'; dst = Clos { term = dst; env = env } }
  | S.Lam t ->
    D.Lam (Clos { term = t; env = env })
  | S.Ap { f; a } -> do_ap (eval f env) (eval a env)

and read_back size pack = 
  match pack with
  | D.Pack { tp = D.Pi { src; dst }; term } -> 
    let x = D.make_var size src in 
    let tp' = do_clos dst x in
    let tm' = do_ap term x in
    S.Lam (read_back (size + 1) (Pack { term = tm'; tp = tp' }))
  | D.Pack { tp = _; term = D.Neutral { term; tp = _ } } -> read_back_ne size term
  | D.Pack { tp = _; term = D.T } -> S.T (* actually this cannot happen *)
  | D.Pack { tp = _; term = D.Pi { src = _; dst = _ } }  ->
    raise @@ NbeError ("read back Pi should not happen")
  | _ -> raise @@ NbeError ("unmatched read back should not happen")

and read_back_ne size ne =
  match ne with
  | D.Var k -> S.Var (size - 1 - k)
  | D.Ap { f; a } -> 
    let f' = read_back_ne size f in
    let a' = read_back size a in
    S.Ap { f = f'; a = a' }