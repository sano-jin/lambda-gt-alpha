(lang dune 3.0)

(name lambda_gt_alpha)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Jin Sano")

(maintainers "Jin Sano")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name lambda_gt_alpha)
 (synopsis "Lambda GT")
 (description "A reference interpreter of the untyped Lambda GT language")
 (depends
  ; (ounit :with-test)
  (menhir
   (>= 2.1))
  ; (patdiff (>= 0.14.0))
  (dune
   (> 1.5))
  (odoc
   (>= 1.5.3))
  (base
   (>= 0.16.2))
  (yojson
   (>= 2.0.2)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
