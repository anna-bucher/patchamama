(lang dune 2.9)

(name patchamama)

(wrapped_executables false)

(generate_opam_files true)

(use_standard_c_and_cxx_flags false)

(package
 (depends
  (ocaml
   (= 4.13.1))
  (cmdliner
   (= 1.0.4))
  (bos
   (= 0.2.1)))
 (name patchamama)
 (maintainers "The patchamama programmers")
 (authors "The patchamama programmers")
 (synopsis "Patchamama lua based controllers")
 (homepage "https://github.com/anna-bucher/patchamama")
 (bug_reports "https://github.com/anna-bucher/patchamama/issues"))
