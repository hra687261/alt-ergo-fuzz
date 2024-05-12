(lang dune 2.0)

(allow_approximate_merlin)

(generate_opam_files true)

(package
 (name alt-ergo-fuzz)
 (synopsis "Alt-Ergo-Fuzz: A fuzzer for the Alt-Ergo SMT solver")
 (description "Alt-Ergo-Fuzz: A fuzzer for the Alt-Ergo SMT solver")
 (license "OCamlPro Non-Commercial Purpose License, version 1")
 (depends
  (ocaml
   (>= 4.05.0))
  (dune
   (>= 2.0))
  (alt-ergo-lib
   (>= 2.5.2))
  (crowbar
   (>= 0.2.1))))
