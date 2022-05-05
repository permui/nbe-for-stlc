%{

open Concrete_syntax

%}

%token <string> ID
%token ARROW
%token LPAR
%token RPAR
%token COLON
%token FUN
%token EQ
%token DEF
%token NORMALIZE
%token EOF

%start <com list> prog

%%

prog:
  | s = commands; EOF { List.rev s }
  ;

commands:
  | (* empty *) { [] }
  | s = commands; c = command { c :: s }
  ;

command:
  | DEF; name = ID; COLON; a = e; EQ; b = e;
    { Def (name, a, b) }
  | NORMALIZE; name = ID
    { Normalize name }
  ;

e:
  | t = e1 { t }
  ;

e1:
  | FUN; x = ID; ARROW; b = e1
    { Lam (Binding { names = [x]; body = b }) }
  | c = e2
    { c }
  ;

e2:
  | a = e3; ARROW; b = e2
    { Pi { src = a; dst = Binding { names = [""]; body = b } } }
  | LPAR; x = ID; COLON; a = e; RPAR; ARROW; b = e2
    { Pi { src = a; dst = Binding { names = [x]; body = b } } }
  | c = e3
    { c }
  ;

e3:
  | f = e3; a = e4
    { Ap { f; a } }
  | c = e4
    { c }
  ;

e4:
  | s = ID
    { Ident s }
  | LPAR; c = e; RPAR
    { c }
  ;
