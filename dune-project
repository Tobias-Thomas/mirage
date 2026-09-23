(lang dune 3.23)

(name mirage)

(generate_opam_files true)

(source
 (github Tobias-Thomas/mirage))

(authors "Tobias Thomas <tobias.thomas@tu-darmstadt.de>")

(package
 (name mirage)
 (synopsis "A probabilistic programming language written in OCaml")
 (depends
  ocaml
  ppx_deriving
  (ppx_expect :with-test))
 (tags
  ("probabilistic programming" "Bayesian inference")))
