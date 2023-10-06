(lang dune 3.7)

(name producer)

(generate_opam_files true)

(source
 (github sixstring982/producer))

(authors "Trent Small")

(maintainers "sixstring982@gmail.com")

(license BSD-3-Clause)

(documentation https://sixstring982.github.io/producer)

(package
 (name producer)
 (synopsis "Accumulate results using monadic dependency graphs")
 (depends (ocaml (>= 4.08)) (dune (>= 3.7)) (alcotest (>= 1.7.0 :with-test)) (hmap (>= 0.8.1)))
 (tags
  ("dependency injection" graph node producer)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
