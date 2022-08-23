# NbE for STLC

This is a practice project aimed at implementing the *Normalization by Evaluation* for *Simply Typed Lambda Calculus*.
It computes the beta-reduced-eta-long normal form of typed lambda expressions.

The author studied Daniel Gratzer's [nbe-for-mltt](https://github.com/jozefg/nbe-for-mltt) before this, so the style of this project is similar to Daniel's.

Terms of our language include 

* Variable: `x`
* Lambda Abstraction: `fun x -> ...`
* Function application: `f x`
* Annotation: `x :: t`

Types of our language include

* User specified base types, like `A`, `B`, etc.
* Function (Arrow) type: `A -> B`

Commands of our language include

* Base type: `basetype A`
* Definition: `def x : A = b`, where `x` is an identifier, `A` and `b` are terms.
* Normalization: `normalize name`, where `name` is some identifier defined by `def`.

See the `example` directory for program examples. Here is one:

```
basetype A
basetype B

def app_id : A -> B = ((fun x -> x) :: (A -> B) -> (A -> B)) (fun y -> y)

def func_id : (A -> A) -> (A -> A) = fun x -> x

normalize app_id
normalize func_id
```

## How to build

The author's system environment is 

* OCaml 4.13.1
* Dune 3.0.3
* sexplib 0.15.0
* Cmdliner 1.1.0

The code should build and run in similar environment, but the author hasn't tested it.

To build the code, first clone the repository. Then go to the `nbe-for-stlc` directory. Install the dependencies and build with the following commands.

``` bash
opam install --deps-only .
dune build
```

## How to run

``` bash
dune exec stlc <filename>
```
