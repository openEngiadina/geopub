(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (guix build-system dune)
 ((guix licenses) #:prefix license:)
 (gnu packages multiprecision)
 (gnu packages ocaml))

(define-public geopub
  (package
    (name "geopub")
    (version "0.0.0")
    (source #f)
    (build-system dune-build-system)
    (arguments '())
    (native-inputs
     `(("alcotest" ,ocaml-alcotest)
       ("qcheck" ,ocaml-qcheck)
       ("js_of_ocaml" ,ocaml-js-of-ocaml)))
    (propagated-inputs
     `(("uri" ,ocaml-uri)))
    (home-page "https://gitlab.com/openengiadina/geopub")
    (synopsis #f)
    (description #f)
    (license license:agpl3+)))

geopub
