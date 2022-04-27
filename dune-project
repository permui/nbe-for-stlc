(lang dune 3.0)

(name nbe_stlc)

(generate_opam_files true)

(source
 (github permui/nbe-for-stlc))

(authors "Aoyang Yu")

(package
 (name nbe_stlc)
 (synopsis "Practise Normalization by Evaluation on STLC.")
 (depends ocaml dune)
 (tags
  ("NbE" "STLC" "Type Theory")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
