(lang dune 3.15)

(name xlsx2csv)

(generate_opam_files true)

(source
 (github zazedd/xlsx2csv))

(authors "Leonardo Santos")

(maintainers "leomendesantos@gmail.com")

(license GPLv3)

(documentation https://url/to/documentation)

(package
 (name xlsx2csv)
 (synopsis "Simple library to export .xlsx files as CSVs")
 (description "Simple library to export .xlsx files as CSVs")
 (depends ocaml dune camlzip xmlm uutf)
 (tags
  (convert xlsx csv)))
