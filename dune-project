(lang dune 3.0)

(name nbe_stlc)

(generate_opam_files true)

(source
 (github permui/nbe-for-stlc))

(authors "Aoyang Yu")
(maintainers "Aoyang Yu")

(package
 (name nbe_stlc)
 (synopsis "Practise Normalization by Evaluation on STLC")
 (depends ocaml dune (sexplib (= 0.15.0)) (cmdliner (= 1.1.0)))
 (tags
  ("NbE" "STLC" "Type Theory")))

(using menhir 2.1)


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
