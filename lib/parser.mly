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
%token BASETYPE
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
  | BASETYPE; name = ID
    { Basetype name }
  | DEF; name = ID; COLON; ty = ty_e; EQ; tm = tm_e
    { Def (name, ty, tm) }
  | NORMALIZE; name = ID
    { Normalize name }
  ;

tm_e:
  | t = tm_e1 { t }
  ;

tm_e1:
  | FUN; x = ID; ARROW; b = tm_e1
    { Lam (Binding { names = [x]; body = b }) }
  | c = tm_e2
    { c }
  ;

tm_e2:
  | f = tm_e2; a = tm_e3
    { Ap { f; a } }
  | c = tm_e3
    { c }
  ;

tm_e3:
  | s = ID
    { TermIdent s }
  | LPAR; c = tm_e; RPAR
    { c }
  ;

ty_e:
  | ty = ty_e1 { ty }
  ;

ty_e1:
  | a = ty_e2; ARROW; b = ty_e1
    { Arrow (a, b) }
  | c = ty_e2
    { c }
  ;

ty_e2:
  | s = ID
    { TypeIdent s }
  | LPAR; c = ty_e; RPAR
    { c }
  ;