(lang dune 3.19)

(name alsdiff)

(version 0.0.1-dev)

(source
 (github krfantasy/alsdiff))

(authors "Jay Xu <jay.xu.krfantasy@gmail.com>")

(maintainers "Jay Xu <jay.xu.krfantasy@gmail.com>")

(license THE-LICENSE.txt)

(documentation https://github.com/krfantasy/alsdiff)

(package
 (name alsdiff)
 (synopsis "Git helper for Ableton Live Set(.als)")
 (description "Git helper for Ableton Live Set(.als)")
 (depends
   (ocaml (>= 5.0.0))
    re
    camlzip
    xmlm
    angstrom
   (cmdliner (>= 2.0.0))
    dune-build-info
   (ocp-indent :build)
    eio
    eio_main
    alcotest
    ppx_deriving
    ppx_deriving_yojson
    ppx_deriving_jsonschema
    jsonschema
    fmt
    ptime)
 (tags
  ("ableton live" "git" "version control")))

(generate_opam_files true)

(formatting disabled)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
