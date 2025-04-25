(lang dune 3.18)

(name float_option)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Stefan Muenzel <source@s.muenzel.net>")

(maintainers "Stefan Muenzel <source@s.muenzel.net>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name float_option)
 (synopsis "A float option library")
 (description "A float option library with an efficient representation")
 (depends ocaml)
 (tags
  (float)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
