(lang dune 3.10)

(name ipcf)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ipcf)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_deriving core)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
