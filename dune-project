(lang dune 3.0)

(name lambda_gt_beta)

(generate_opam_files true)

(source
 (github jin-sano/lambda-gt-alpha))

(authors "Jin Sano")

(maintainers "Jin Sano")

(license MIT)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name lambda_gt_beta)
 (synopsis "Lambda GT")
 (description "A reference interpreter of the untyped Lambda GT language")
 (depends
  (ocaml
   (>= 4.14.0))
  ; (ounit :with-test)
  (menhir
   (>= 2.1))
  ; (patdiff (>= 0.14.0))
  (dune
   (> 1.5))
  (odoc
   (>= 1.5.3)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
