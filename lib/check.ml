module S = Syntax
module D = Domain

exception TypeError of string

let rec check term sem_ty tps =
  match term with
  | S.Lam t ->
    begin
      match sem_ty with
      | D.Arrow (t1, t2) -> check t t2 (t1 :: tps)
      | D.Base _ -> raise @@ TypeError ("lambda term checking base type")
    end
  | _ -> 
    begin
      match synth term tps with
      | Some t -> if t = sem_ty then () else raise @@ TypeError ("synthesized type and checking type mismatch")
      | None -> raise @@ TypeError ("cannot synthesize type")
    end

and synth term tps = 
  match term with
  | S.Var k -> Some (List.nth tps k)
  | S.Ap { f; a } ->
    let st = synth f tps in
    begin
      match st with
      | Some (D.Base _) -> raise @@ TypeError ("function synthesized base type")
      | Some (D.Arrow (s, t)) -> 
        check a s tps; Some t
      | None -> None
    end
  | _ -> None
