(lang dune 3.18)

(name float_option)

(generate_opam_files true)

(source
 (github engineeredabstraction/float_option))

(authors "Stefan Muenzel <source@s.muenzel.net>")

(maintainers "Stefan Muenzel <source@s.muenzel.net>")

(license MPL-2.0)

(package
 (name float_option)
 (synopsis "A float option library")
 (description "A float option library with an efficient representation")
 (depends ocaml ppx_sexp_conv ppx_optional)
 (tags
  (float)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
