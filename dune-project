(lang dune 3.20)

(name AdventOfCodeOCaml)

(generate_opam_files true)

(source
 (github Matematik411/AdventOfCodeOCaml))

(authors "Nejc Zajc")

(license MIT)

(package
 (name AdventOfCodeOCaml)
 (synopsis "Repository for solving Advent of Code challenges in OCaml")
 (description "This repository contains solutions to the Advent of Code challenges implemented in OCaml.")
 (depends
  digestif))
