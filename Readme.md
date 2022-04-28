# NbE for STLC

This is a practice project aimed at implementing the *Normalization by Evaluation* for *Simply Typed Lambda Calculus*.
It computes the beta-reduced-eta-long normal form of typed lambda expressions.

Terms of our language include 

* Variable: `x`
* Base type: `T` (literally, which means there is only one base type, `T`)
* Non-dependent function type: `A -> B`
* Dependent function type: `(x: A) -> B`
* Function application: `f x`

Commands of our language include

* Definition: `def x : A = b`, where `x` is an identifier, `A` and `b` are terms.
* Normalization: `normalize name`, where `name` is some identifier defined by `def`.

See `test/test.st` for some examples.

## How to build

The author's system environment is 

* OCaml 4.13.1
* Dune 3.0.3
* sexplib 0.15.0

The code should build and run in similar environment, but the author hasn't tested it.

To build the code, first clone the repository. Then go to the `nbe-for-stlc` directory. Install the dependencies and build with the following commands.

``` bash
opam install --deps-only .
dune build
```

## How to run

``` bash
dune exec stlc
```

Currently, the input file is `test/test.st` and its path is hard-coded in `main.ml`. This behavior will (of course) be replaced by
better command line interface in the future. 

## Notes

The code has not been fully tested, but it will be.