open Utils
module CS = Concrete_syntax
module S  = Syntax
module D  = Domain

type env = Env of { len: int; names: string list; terms: D.t list; tps: D.t list }

(* we ignore typecheck for now *)
let work com (Env { len; names; terms; tps }) =
  match com with
  | CS.Def (name, tp, term) ->
    let tp = S.remove_names tp names in
    let term = S.remove_names term names in
    let sem_tp = Nbe.eval tp terms in
    let sem_term = Nbe.eval term terms in
    Env { len = len + 1; names = name :: names; terms = sem_term :: terms; tps = sem_tp :: tps }
  | CS.Normalize name ->
    begin 
      match get_index names name with
      | None -> Printf.printf "normalize name `%s` unbound\n" name
      | Some k -> 
        let term = List.nth terms k in
        let tp = List.nth tps k in
        let nf = Nbe.read_back 0 (Pack { term; tp }) in
        Printf.printf "normalize %s =\n  " name;
        let (_, cnf) = S.to_concrete_syntax nf in 
        CS.print_t cnf
    end;
    Env { len; names; terms; tps }

let empty_env = Env { len = 0; names = []; terms = []; tps = [] }

let rec run_with_env coms env =
  match coms with
  | com :: coms -> 
    let env = work com env in
    run_with_env coms env
  | [] -> ()

let run coms = 
  try run_with_env coms empty_env with
  | S.MyError s -> Printf.printf "S.MyError: %s\n" s