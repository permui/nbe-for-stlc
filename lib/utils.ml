
let get_index l x = 
  let rec get_index_ l x k = 
    match l with
    | [] -> None
    | y :: t -> if x = y then Some k else get_index_ t x (k + 1)
  in
  get_index_ l x 0

