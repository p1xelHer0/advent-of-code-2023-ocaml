(lang dune 3.11)

(name aoc)

(generate_opam_files true)

(authors "p1xelHer0")

(maintainers "p1xelHer0")

(homepage "https://github.com/p1xelHer0/advent-of-code-2023-ocaml")

(bug_reports "https://github.com/p1xelHer0/advent-of-code-2023-ocaml/issues")

(license MIT)

(package
 (name aoc)
 (synopsis "Advent of Code 2023 in OCaml")
 (description
  "This project contains my solutions to Advent of Code 2023 written in OCaml")
 (depends
  (ocaml
   (>= 5.1.0))
  (dune
   (and
    :build
    (>= 3.11.1)))
  (angstrom
   (>= 0.15.0))
  (containers
   (>= 3.12))
  (fmt
   (>= 0.9.0))
  (oseq
   (>= 0.5))
  (psq
   (>= 0.2.1))
  (ppx_deriving
   (>= 5.2.1))
  (alcotest
   (and
    :with-test
    (>= 1.7.0)))
  (ppx_inline_alcotest
   (and
    :with-test
    (>= 1.0.0)))
  (ocaml-lsp-server
   (and
    :with-dev-setup
    (>= 1.16.2)))
  (ocamlformat
   (and
    :with-dev-setup
    (>= 0.26.1)))
  (utop
   (and
    :with-dev-setup
    (>= 2.13.1)))))
