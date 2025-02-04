(lang dune 3.15)

(generate_opam_files true)

(name synopsis)

(package
  (name synopsis)
  (authors "cliff" "ptrichr")
  (maintainers "cliff" "ptrichr")
  (synopsis "Gather information about module usage, top-level bindings, and function calls")
  (license MIT)
  (source
    (github umd-cmsc330/restriction-checker))
  (depends 
    (ocaml (>= 5.2.0)) 
    dune)
  (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/latest/reference/dune-project/index.html
