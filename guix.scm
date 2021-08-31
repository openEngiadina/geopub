(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (guix build-system ocaml)
 (guix build-system dune)
 ((guix licenses) #:prefix license:)
 (gnu packages multiprecision)
 (gnu packages ocaml))

(define-public ocaml-note
  (package
    (name "ocaml-note")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://erratique.ch/software/note/releases/note-0.0.1.tbz")
       (sha256
        (base32
         "1mb32nq39bzkxbk5bkcpi8r344d951crrkrdph79j5zs8lbplqhw"))))
    (build-system ocaml-build-system)
    (arguments `(#:build-flags (list "build" "--tests" "true")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)
       ("ocamlbuild" ,ocamlbuild)
       ("ocaml-topkg" ,ocaml-topkg)
       ("opam" ,opam)))
    (home-page "https://erratique.ch/software/note")
    (synopsis
     "Declarative events and signals for OCaml")
    (description
     "
Note is an OCaml library for functional reactive programming (FRP). It
provides support to program with time varying values: declarative
events and signals.

Note is distributed under the ISC license.
")
    (license license:isc)))

(define-public ocaml-brr
  (package
    (name "ocaml-brr")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://erratique.ch/software/brr/releases/brr-0.0.1.tbz")
        (sha256
          (base32
            "1kaydz0cc8f2qlm88ifywzh88packqr0q70lda5p2f4vhcbxfp4h"))))
    (build-system ocaml-build-system)
    (arguments `(#:build-flags (list "build" "--tests" "true")
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (propagated-inputs
     `(("js-of-ocaml" ,js-of-ocaml)
       ("ocaml-note" ,ocaml-note)))
    (native-inputs
      `(("ocaml-findlib" ,ocaml-findlib)
        ("ocamlbuild" ,ocamlbuild)
        ("ocaml-topkg" ,ocaml-topkg)
        ("opam" ,opam)))
    (home-page "https://erratique.ch/software/brr")
    (synopsis "Browser programming toolkit for OCaml")
    (description
      "Brr is a toolkit for programming browsers in OCaml with the
[`js_of_ocaml`][jsoo] compiler. It provides:

* Interfaces to a selection of browser APIs.
* Note based reactive support (optional and experimental).
* An OCaml console developer tool for live interaction
  with programs running in web pages.
* A JavaScript FFI for idiomatic OCaml programming.

Brr is distributed under the ISC license. It depends on [Note][note]
and on the `js_of_ocaml` compiler and runtime – but not on its\nlibraries or syntax extension.

[note]: https://erratique.ch/software/note
[jsoo]: https://ocsigen.org/js_of_ocaml
")
    (license license:isc)))

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
       ("ocaml-react" ,ocaml-react)
       ("ocaml-lwt-react" ,ocaml-lwt-react)
       ("ocaml-brr" ,ocaml-brr)
       ("js_of_ocaml" ,js-of-ocaml)))
    (propagated-inputs
     `(("uri" ,ocaml-uri)))
    (home-page "https://gitlab.com/openengiadina/geopub")
    (synopsis #f)
    (description #f)
    (license license:agpl3+)))

geopub
