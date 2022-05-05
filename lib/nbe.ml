module S = Syntax
module D = Domain

exception NbeError of string

let rec do_clos (D.Clos { term; env }) a = eval_t term (a :: env)

and do_ap f a =
  match f with
  | D.Lam clos -> do_clos clos a
  | D.Neutral { term; tp = D.Arrow (t1, t2) } ->
    D.Neutral { term = D.Ap { f = term; a = Pack { term = a; tp = t1 } }; tp = t2 }
  | _ -> raise @@ NbeError ("not a function in do_ap")

and eval_t t env = 
  match t with
  | S.Annot (tm, _ty) -> eval_t tm env
  | S.Var k -> List.nth env k
  | S.Lam t ->
    D.Lam (Clos { term = t; env = env })
  | S.Ap { f; a } -> do_ap (eval_t f env) (eval_t a env)

and eval_ty ty =
  match ty with
  | S.Base k -> D.Base k
  | S.Arrow (t1, t2) -> D.Arrow (eval_ty t1, eval_ty t2)

and read_back size pack = 
  match pack with
  | D.Pack { term; tp = D.Arrow (t1, t2) } -> 
    let x = D.make_var size t1 in
    let p = D.Pack { term = do_ap term x; tp = t2 } in 
    S.Lam (read_back (size + 1) p)
  | D.Pack { term = D.Neutral { term; tp = _ }; tp = D.Base _ } ->
    read_back_ne size term
  | _ -> raise @@ NbeError ("unmatched read back should not happen")

and read_back_ne size ne =
  match ne with
  | D.Var k -> S.Var (size - 1 - k)
  | D.Ap { f; a } -> 
    let f' = read_back_ne size f in
    let a' = read_back size a in
    S.Ap { f = f'; a = a' }