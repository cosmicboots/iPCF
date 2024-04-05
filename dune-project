(lang dune 3.10)

(name ipcf)

(generate_opam_files true)

(source
 (github cosmicboots/ipcf))

(authors "Thom Dickson")

(maintainers "Thom Dickson")

(license LICENSE)

(package
 (name ipcf)
 (synopsis "An iPCF interpreter")
 (description
  "An interpreter for the iPCF language, a simple extension of the simply-typed lambda calculus with intentionality.")
 (depends
  ocaml
  dune
  ppx_deriving
  core
  cmdliner
  ocamline
  ppx_inline_test
  ANSITerminal)
 (tags
  (ipcf pcf "lambda calculus")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
