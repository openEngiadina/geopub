(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (guix build-system ocaml)
 (guix build-system dune)
 ((guix licenses) #:prefix license:)
 (gnu packages multiprecision)
 (gnu packages pkg-config)
 (gnu packages ocaml))

(define-public ocaml-cbor
  (package
    (name "ocaml-cbor")
    (version "11261798db015a768d4759b33a529395e2ab5a30")
    (home-page "https://inqlab.net/git/ocaml-cbor.git")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url home-page)
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32 "12l3xz1jpnw87w0k506qh2hv8aiw0sq2fz5ri8rka1glizhb5wvn"))))
    (build-system dune-build-system)
    (native-inputs
     `(("alcotest" ,ocaml-alcotest)
       ("qcheck" ,ocaml-qcheck)))
    (propagated-inputs
     `(("angstrom" ,ocaml-angstrom)
       ("zarith" ,ocaml-zarith)
       ("gmp" ,gmp)))
    (synopsis #f)
    (description #f)
    (license license:agpl3+)))

(define-public ocaml-rdf
  (package
    (name "ocaml-rdf")
    (version "212b22c60ea53da0667cad4a6c75e50fa2c6df63")
    (home-page "https://inqlab.net/git/ocaml-rdf.git")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
	    (url home-page)
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0bi32gip7qsb8vf2xk492j46c7zaj2pib7rp17kmmbyv5zczna3l"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (native-inputs
     `(("alcotest" ,ocaml-alcotest)
       ("qcheck" ,ocaml-qcheck)))
    (propagated-inputs
     `(("ocaml-uri" ,ocaml-uri)
       ("yojson" ,ocaml-yojson)
       ("cbor" ,ocaml-cbor)
       ("angstrom" ,ocaml-angstrom)))
    (synopsis "RDF library for OCaml")
    (description #f)
    (license license:agpl3+)))

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

(define-public ocaml-digestif
  (package
    (name "ocaml-digestif")
    (version "1.0.0")
    (home-page "https://github.com/mirage/digestif")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0x046by4myiksch16vyhj5l7xkflwhhxm8gzlf7474y0mw77w6lw"))))
    (build-system dune-build-system)
    (propagated-inputs
     `(("ocaml-eqaf" ,ocaml-eqaf)
       ("ocaml-bigarray-compat" ,ocaml-bigarray-compat)
       ("ocaml-stdlib-shims" ,ocaml-stdlib-shims)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("ocaml-fmt" ,ocaml-fmt)
       ("ocaml-alcotest" ,ocaml-alcotest)
       ("ocaml-bos" ,ocaml-bos)
       ("ocaml-astring" ,ocaml-astring)
       ("ocaml-fpath" ,ocaml-fpath)
       ("ocaml-rresult" ,ocaml-rresult)
       ("ocaml-findlib" ,ocaml-findlib)))
    (synopsis "OCaml implementations of various hash functions (SHA*, RIPEMD160,
BLAKE2* and MD5)")
    (description
     "Digestif is a toolbox to provide hashes implementations in C and OCaml.

It uses the linking trick and user can decide at the end to use the C implementation or the OCaml implementation.

We provides implementation of:
 * MD5
 * SHA1
 * SHA224\n * SHA256
 * SHA384
 * SHA512
 * BLAKE2B
 * BLAKE2S
 * RIPEMD160
")
    (license license:expat)))

(define-public ocaml-logs
  (package
    (name "ocaml-logs")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/logs/releases/"
                                  "logs-" version ".tbz"))
              (sha256
                (base32
                  "1jnmd675wmsmdwyb5mx5b0ac66g4c6gpv5s4mrx2j6pb0wla1x46"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build" "--with-js_of_ocaml" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("ocamlbuild" ,ocamlbuild)
       ("opam" ,opam)))
    (propagated-inputs
     `(("fmt" ,ocaml-fmt)
       ("lwt" ,ocaml-lwt)
       ("mtime" ,ocaml-mtime)
       ("result" ,ocaml-result)
       ("cmdliner" ,ocaml-cmdliner)
       ("js-of-ocaml" ,js-of-ocaml)
       ("topkg" ,ocaml-topkg)))
    (home-page "https://erratique.ch/software/logs")
    (synopsis "Logging infrastructure for OCaml")
    (description "Logs provides a logging infrastructure for OCaml.  Logging is
performed on sources whose reporting level can be set independently.  Log
message report is decoupled from logging and is handled by a reporter.")
    (license license:isc)))

(define-public ocaml-markup-lwt
  (package
    (inherit ocaml-markup)
    (name "ocaml-markup-lwt")
    (arguments `(#:package "markup-lwt"))
    (propagated-inputs
     `(("ocaml-bisect-ppx" ,ocaml-bisect-ppx)
       ("ocaml-uchar" ,ocaml-uchar)
       ("ocaml-uutf" ,ocaml-uutf)
       ("ocaml-lwt" ,ocaml-lwt)
       ("ocaml-markup" ,ocaml-markup)))))

(define-public ocaml-xmpp
  (package
    (name "ocaml-xmpp")
    (version "17d7b7438a3b108de5390bea9b2c7267b56c5266")
    (home-page "https://inqlab.net/git/ocaml-xmpp.git")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url home-page)
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32 "1n977yq6nw22y01305ajjsah2z4njkgvw651b3b2x5kkznnbrpqz"))))
    (arguments `(#:tests? #f))
    (build-system dune-build-system)
    (native-inputs
     `(("alcotest" ,ocaml-alcotest)
       ("qcheck" ,ocaml-qcheck)))
    (propagated-inputs
     `(("ocaml-lwt" ,ocaml-lwt)
       ("ocaml-logs" ,ocaml-logs)
       ("ocaml-fmt" ,ocaml-fmt)
       ("ocaml-uunf" ,ocaml-uunf)
       ("ocaml-cmdliner" ,ocaml-cmdliner)
       ("ocaml-lwt-react" ,ocaml-lwt-react)
       ("ocaml-angstrom" ,ocaml-angstrom)
       ("ocaml-markup" ,ocaml-markup)
       ("ocaml-markup-lwt" ,ocaml-markup-lwt)
       ("ocaml-xmlm" ,ocaml-xmlm)
       ("ocaml-digestif" ,ocaml-digestif)
       ("ocaml-cstruct" ,ocaml-cstruct)
       ("ocaml-base64" ,ocaml-base64)
       ("ocaml-brr" ,ocaml-brr)
       ("js-of-ocaml" ,js-of-ocaml)))
    (synopsis #f)
    (description #f)
    (license license:agpl3+)))

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
       ("ocaml-rdf" ,ocaml-rdf)
       ("ocaml-xmpp" ,ocaml-xmpp)
       ("js_of_ocaml" ,js-of-ocaml)))
    (propagated-inputs
     `(("uri" ,ocaml-uri)))
    (home-page "https://gitlab.com/openengiadina/geopub")
    (synopsis #f)
    (description #f)
    (license license:agpl3+)))

geopub
