open Utils
module CS = Concrete_syntax
module S  = Syntax
module D  = Domain

exception DriverError of string

type env = Env of {
  len: int;
  bases: string list;
  names: string list;
  terms: D.t list;
  tps: D.ty list
}

(* we ignore typecheck for now *)
let work com (Env { len; bases; names; terms; tps }) =
  match com with
  | CS.Basetype s ->
    if List.mem s bases
      then raise (DriverError ("duplicated basetype `" ^ s ^ "`"))
      else Env { len; bases = s :: bases; names; terms; tps }
  | CS.Def (name, tp, term) ->
    let tp = S.remove_names_ty tp bases in
    let term = S.remove_names_t term names bases in
    let sem_tp = Nbe.eval_ty tp in
    Check.check term sem_tp tps;
    let sem_term = Nbe.eval_t term terms in
    Env { len = len + 1; bases; names = name :: names; terms = sem_term :: terms; tps = sem_tp :: tps }
  | CS.Normalize name ->
    begin 
      match get_index names name with
      | None -> Printf.printf "normalize name `%s` unbound\n" name
      | Some k -> 
        let term = List.nth terms k in
        let tp = List.nth tps k in
        let nf = Nbe.read_back 0 (Pack { term; tp }) in
        Printf.printf "normalize %s =\n  " name;
        let (_, cnf) = S.t_to_concrete_syntax nf in 
        CS.print_t cnf
    end;
    Env { len; bases; names; terms; tps }

let empty_env = Env { len = 0; bases = []; names = []; terms = []; tps = [] }

let rec run_with_env coms env =
  match coms with
  | com :: coms -> 
    let env = work com env in
    run_with_env coms env
  | [] -> ()

let run coms = 
  try run_with_env coms empty_env with
  | S.Error s -> Printf.printf "S.Error: %s\n" s
  | DriverError s -> Printf.printf "DriverError: %s\n" s