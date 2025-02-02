(lang dune 3.15)

(name synopsis)

(generate_opam_files true)

(source
 (github ptrichr/synopsis))

(authors ["cliff" "ptrichr"])

(maintainers ["cliff" "ptrichr"])

(package
 (name synopsis)
 (synopsis "Gather information about module usage, top-level bindings, and function calls")
 (description "Used in CMSC330 to determine whether or not student submissions have met restrictions imposed by project guidelines. Powered by the OCaml compilerlib parsetree module.")
 (depends ocaml dune)
 (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/latest/reference/dune-project/index.html
