(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (guix build-system dune)
 (guix build-system ocaml)
 ((guix licenses) #:prefix license:)
 (gnu packages base)
 (gnu packages maths)
 (gnu packages multiprecision)
 (gnu packages pkg-config)
 (gnu packages ocaml)
 (gnu packages rdf))

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
    (version "9aae0dbd86818a897015ed8e050b79bdafeb706a")
    (home-page "https://codeberg.org/openEngiadina/ocaml-rdf.git")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
	    (url home-page)
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0y4ry6y5fn1f7k893hxyfixpzz6g2gxyli0h2krk22vc580acs88"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (native-inputs
     `(("alcotest" ,ocaml-alcotest)
       ("qcheck" ,ocaml-qcheck)))
    (propagated-inputs
     `(("ocaml-uri" ,ocaml-uri)
       ("ocaml-yojson" ,ocaml-yojson)
       ("ocaml-cbor" ,ocaml-cbor)
       ("ocaml-angstrom" ,ocaml-angstrom)
       ("ocaml-ctypes" ,ocaml-ctypes)
       ("ocaml-xmlm" ,ocaml-xmlm)
       ("ocaml-uunf" ,ocaml-uunf)
       ("ocaml-uuidm" ,ocaml-uuidm)
       ("ocaml-z3" ,ocaml-z3)
       ("z3" ,z3)
       ("serd" ,serd)))
    (synopsis "RDF library for OCaml")
    (description #f)
    (license license:agpl3+)))

(define-public ocaml-ptime
  (package
  (name "ocaml-ptime")
  (version "0.8.5")
  (source
    (origin
      (method url-fetch)
      (uri "https://erratique.ch/software/ptime/releases/ptime-0.8.5.tbz")
      (sha256
        (base32
          "1fxq57xy1ajzfdnvv5zfm7ap2nf49znw5f9gbi4kb9vds942ij27"))))
  (build-system ocaml-build-system)
  (arguments
   `(#:build-flags (list "build" "--with-js_of_ocaml" "true" "--tests" "true")
     #:phases
     (modify-phases %standard-phases
       (delete 'configure))))
  (propagated-inputs
   `(("ocaml-result" ,ocaml-result)
     ("js-of-ocaml" ,js-of-ocaml)))
  (native-inputs
    `(("ocaml-findlib" ,ocaml-findlib)
      ("ocamlbuild" ,ocamlbuild)
      ("ocaml-topkg" ,ocaml-topkg)
      ("opam" ,opam)))
  (home-page "https://erratique.ch/software/ptime")
  (synopsis "POSIX time for OCaml")
  (description
    "Ptime offers platform independent POSIX time support in pure OCaml. It
provides a type to represent a well-defined range of POSIX timestamps
with picosecond precision, conversion with date-time values,
conversion with [RFC 3339 timestamps][rfc3339] and pretty printing to a
human-readable, locale-independent representation.")
  (license license:isc)))

(define-public ocaml-syndic
  (package
    (name "ocaml-syndic")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/Cumulus/Syndic/releases/download/v1.6.1/syndic-v1.6.1.tbz")
       (sha256
        (base32
         "1i43yqg0i304vpiy3sf6kvjpapkdm6spkf83mj9ql1d4f7jg6c58"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("ocaml-ptime" ,ocaml-ptime)
       ("ocaml-uri" ,ocaml-uri)
       ("ocaml-xmlm" ,ocaml-xmlm)))
    (native-inputs
     `(
       ;; ("ocaml-fmt" ,ocaml-fmt)
       ;; ("ocaml-ocurl" ,ocaml-ocurl)
       ;; ("ocaml-fpath" ,ocaml-fpath)
       ;; ("ocaml-ocplib-json-typed" ,ocaml-ocplib-json-typed)
       ;; ("ocaml-base-unix" ,ocaml-base-unix)
       ;; ("ocaml-jsonm" ,ocaml-jsonm)
       ))
    (home-page "https://github.com/Cumulus/Syndic")
    (synopsis "RSS1, RSS2, Atom and OPML1 parsing")
    (description
     "Pure OCaml Library for parsing and writing various types of
feeds and subscriber lists.")
    (license license:expat)))

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
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri
	 (string-append
	  "https://erratique.ch/software/brr/releases/brr-" version
	  ".tbz"))
	(sha256
          (base32
            "1xf2sq2rszx5r2x01gc3krx20yh3wm6hwk15zfav0clm8afjsq6m"))))
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

(define-public ocaml-pprint
  (package
    (name "ocaml-pprint")
    (version "20200410")
    (home-page "https://github.com/fpottier/pprint")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16xc0rd4yj1y9rrs9fbhidd08icy4pc1plx48hp0xs6vnkh1wxjm"))))
    (build-system dune-build-system)
    (synopsis "OCaml pretty-printing combinator library and rendering
engine")
    (description "This OCaml library offers a set of combinators for building
so-called documents as well as an efficient engine for converting documents to
a textual, fixed-width format.  The engine takes care of indentation and line
breaks, while respecting the constraints imposed by the structure of the
document and by the text width.")
    (license license:lgpl2.0)))

(define-public ocaml-crowbar
  (package
    (name "ocaml-crowbar")
    (version "0.2")
    (home-page "https://github.com/stedolan/crowbar")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wjfc9irvirfkic32ivvj6qb7r838w08b0d3vmngigbjpjyc9b14"))))
    (build-system dune-build-system)
    (arguments
     ;; Tests require ocaml-xmldiff which requires OCaml 4.12.0
     `(#:tests? #f))
    (propagated-inputs
     `(("ocaml-ocplib-endian" ,ocaml-ocplib-endian)
       ("ocaml-cmdliner" ,ocaml-cmdliner)
       ("ocaml-afl-persistent" ,ocaml-afl-persistent)))
    (native-inputs
     `(("ocaml-calendar" ,ocaml-calendar)
       ("ocaml-fpath" ,ocaml-fpath)
       ("ocaml-uucp" ,ocaml-uucp)
       ("ocaml-uunf" ,ocaml-uunf)
       ("ocaml-uutf" ,ocaml-uutf)
       ("ocaml-pprint" ,ocaml-pprint)))
    (synopsis "Ocaml library for tests, let a fuzzer find failing cases")
    (description "Crowbar is a library for testing code, combining
QuickCheck-style property-based testing and the magical bug-finding powers of
@uref{http://lcamtuf.coredump.cx/afl/, afl-fuzz}.")
    (license license:expat)))

(define-public ocaml-afl-persistent
  (package
    (name "ocaml-afl-persistent")
    (version "1.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/stedolan/ocaml-afl-persistent")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
           "06yyds2vcwlfr2nd3gvyrazlijjcrd1abnvkfpkaadgwdw3qam1i"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "./build.sh")))
         ;; XXX: The tests are already run in the build.sh script.
         (delete 'check))))
    (native-inputs
     `(("opam" ,opam)))
    (home-page "https://github.com/stedolan/ocaml-afl-persistent")
    (synopsis "Use afl-fuzz in persistent mode")
    (description
      "afl-fuzz normally works by repeatedly forking the program being tested.
Using this package, you can run afl-fuzz in ``persistent mode'', which avoids
repeated forking and is much faster.")
    (license license:expat)))

(define-public ocaml-eqaf
  (package
    (name "ocaml-eqaf")
    (version "0.7")
    (home-page "https://github.com/mirage/eqaf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "06hsnnjax1kb3qsi3cj0nyyz8c2hj2gbw3h517gpjinpnwy2fr85"))))
    (build-system dune-build-system)
    (propagated-inputs
     ;; required to build the eqaf.cstruct library (see https://github.com/mirage/eqaf/pull/27)
     `(("ocaml-bigarray-compat" ,ocaml-bigarray-compat)
       ("ocaml-cstruct" ,ocaml-cstruct)))
    (native-inputs
     `(("ocaml-alcotest" ,ocaml-alcotest)
       ("ocaml-crowbar" ,ocaml-crowbar)))
    (synopsis "OCaml library for constant-time equal function on string")
    (description "This OCaml library provides an equal function on string in
constant-time to avoid timing-attack with crypto stuff.")
    (license license:expat)))

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

(define-public ocaml-lwt-ssl
  (package
    (name "ocaml-lwt-ssl")
    (version "1.1.3")
    (home-page "https://github.com/ocsigen/lwt_ssl")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0v417ch5zn0yknj156awa5mrq3mal08pbrvsyribbn63ix6f9y3p"))))
    (build-system dune-build-system)
    (arguments `(#:test-target "."))
    (propagated-inputs
     `(("ocaml-lwt" ,ocaml-lwt)
       ("ocaml-ssl" ,ocaml-ssl)))
    (properties `((upstream-name . "lwt_ssl")))
    (synopsis "OpenSSL binding for OCaml with concurrent I/O")
    (description "This OCaml library provides an Lwt-enabled wrapper around
@code{ocaml-ssl}, that performs I/O concurrently.")
    (license license:lgpl2.1+))) ; with linking exception

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

(define-public ocaml-alcotest-lwt
  (package
    (inherit ocaml-alcotest)
    (name "ocaml-alcotest-lwt")
    (arguments
     `(#:package "alcotest-lwt"
       #:test-target "."
       ;; TODO fix tests
       #:tests? #f))
    (propagated-inputs
     `(("ocaml-alcotest" ,ocaml-alcotest)
       ("ocaml-lwt" ,ocaml-lwt)
       ("ocaml-logs" ,ocaml-logs)))
    (native-inputs
     `(("ocaml-re" ,ocaml-re)
       ("ocaml-cmdliner" ,ocaml-cmdliner)))))

(define-public ocaml-xmppl
  (package
    (name "ocaml-xmppl")
    (version "ee9c22e19cc4e41b3ddb5dae5c9096df3ba9128f")
    (home-page "https://codeberg.org/openEngiadina/ocaml-xmppl.git")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url home-page)
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32 "1d0yvs6q91nc854ff0hr8rmgxgi1xczjpvfm1b6zfi6k1m0kbbif"))))
    ;; (arguments `(#:tests? #f))
    (build-system dune-build-system)
    (native-inputs
     `(("alcotest" ,ocaml-alcotest)
       ("ocaml-alcotest-lwt" ,ocaml-alcotest-lwt)
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
       ("ocaml-lwt-ssl" ,ocaml-lwt-ssl)
       ("js-of-ocaml" ,js-of-ocaml)))
    (synopsis #f)
    (description #f)
    (license license:agpl3+)))

(define-public ocaml-zarith-stubs-js
  (package
   (name "ocaml-zarith-stubs-js")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/zarith_stubs_js-v0.15.0.tar.gz")
     (sha256
      (base32 "03sk4awj6wgxq740k0132y1f53q7gz8lw4pd9slf4xynhgw34pps"))))
   (build-system dune-build-system)
   (arguments `(#:tests? #f))
   (properties `((upstream-name . "zarith_stubs_js")))
   (home-page "https://github.com/janestreet/zarith_stubs_js")
   (synopsis "Javascripts stubs for the Zarith library")
   (description
    " This library contains no ocaml code, but instead implements all of the Zarith C
stubs in Javascript for use in Js_of_ocaml")
   (license license:expat)))

(define-public ocaml-datalogl
  (package
    (name "ocaml-datalogl")
    (version "18d6be30a757feb17b5a8c180d14e8a1f82a4bb8")
    (home-page "https://codeberg.org/openEngiadina/ocaml-datalogl")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url home-page)
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32 "14x7s2g5qmhznh1dbrywf2bp30f06k22p3ji5y1q2xcy6alxi96m"))))
    (build-system dune-build-system)
    (arguments '())
    (propagated-inputs
     (list ocaml-fmt
	   ocaml-logs
	   ocaml-lwt
	   ocaml-angstrom))
    (native-inputs
     (list ocaml-alcotest ocaml-alcotest-lwt))
    (synopsis "A Datalog library for OCaml")
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
     (list
      ocaml-react
      ocaml-lwt-react
      ocaml-brr
      ocaml-rdf
      ocaml-uri
      ocaml-xmppl
      ocaml-datalogl
      ocaml-ptime
      ocaml-zarith-stubs-js
      js-of-ocaml
      ocaml-merlin
      ocaml-dot-merlin-reader))
    (home-page "https://gitlab.com/openengiadina/geopub")
    (synopsis #f)
    (description #f)
    (license license:agpl3+)))

geopub
