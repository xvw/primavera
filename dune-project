(lang dune 3.20)
(name primavera)
(version dev)
(generate_opam_files)
(executables_implicit_empty_intf)
(using mdx 0.4)

(source (github xvw/primavera))
(license MIT)
(authors
  "gr-im <grimfw@gmail.com>"
  "xvw <xaviervdw@gmail.com>")
(maintainers
  "gr-im <grimfw@gmail.com>"
  "xvw <xaviervdw@gmail.com>")

(package
 (name primavera)
 (synopsis "Dependency injection based on a Reader Monad")
 (description "Primavera offers a Reader monad for injecting dependencies")
 (depends
  (ocaml (>= 5.0.0))
  (ppx_expect :with-test)
  (mdx :with-test)

  ;; dev setup
  (utop :with-dev-setup)
  (ocamlformat :with-dev-setup)
  (ocp-indent :with-dev-setup)
  (merlin :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)))
