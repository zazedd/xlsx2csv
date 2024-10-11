(lang dune 3.15)

(name xlsx2csv)

(generate_opam_files true)

(source
 (github zazedd/xlsx2csv))

(authors "Leonardo Santos")

(maintainers "leomendesantos@gmail.com")

(license "GPL-3.0-only")

; (documentation https://url/to/documentation)

(package
 (name xlsx2csv)
 (synopsis "Simple library to export .xlsx files as CSVs")
 (description "Simple library to export .xlsx files as CSVs")
 (depends
   (ocaml (>= 4.13.0))
   dune
   camlzip
   xmlm
   uutf
   lwt
   fpath)
 (tags
  (convert xlsx csv)))
