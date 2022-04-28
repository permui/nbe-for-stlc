def id : T -> T = fun x -> x

def true : T -> T -> T = fun x -> fun y -> x

def false : T -> T -> T = fun x -> fun y -> y

def ap : (T -> T) -> T -> T = fun f -> fun x -> f x

def func_id : (T -> T) -> (T -> T) = fun f -> f

normalize id
normalize true
normalize false
normalize ap
normalize func_id