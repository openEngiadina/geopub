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
 (gnu packages pv)
 (gnu packages ocaml)
 (gnu packages rdf)
 (gnu packages zig))

(define-public ocaml-crunch
  (package
   (name "ocaml-crunch")
   (version "3.2.0")
   (home-page "https://github.com/mirage/ocaml-crunch")
   (source (origin
            (method git-fetch)
            (uri (git-reference
		  (url home-page)
		  (commit (string-append "v" version))))
            (sha256
             (base32
              "0xsa9gciszz66q8firfzqxdg7h9v8l95xp86ngr5p4sljrj03cx2"))))
   (build-system dune-build-system)
   ;; require mirage-kv
   (arguments '(#:tests? #f))
   (propagated-inputs (list ocaml-cmdliner ocaml-ptime))
   (native-inputs (list ocaml-lwt))
   (synopsis "Convert a filesystem into a static OCaml module")
   (description
    "`ocaml-crunch` takes a directory of files and compiles them into a standalone
OCaml module which serves the contents directly from memory.  This can be
convenient for libraries that need a few embedded files (such as a web server)
and do not want to deal with all the trouble of file configuration.")
   (license license:isc)))

(define-public ocaml-decoders
  (package
   (name "ocaml-decoders")
   (version "0.7.0")
   (home-page "https://github.com/mattjbray/ocaml-decoders")
   (source (origin
            (method git-fetch)
            (uri (git-reference
		  (url home-page)
		  (commit (string-append "v" version))))
            (sha256
             (base32
              "1h5q66nlapbjyqpjn93zpiwdvb9b2kxxgqw2jxyp6a5y815k5hfj"))))
   (build-system dune-build-system)
   (arguments `(#:tests? #f
		#:package "decoders"))
   (synopsis "Elm-inspired decoders for Ocaml")
   (description
    "This package provides a combinator library for \"decoding\" JSON-like values into
your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
   (license license:isc)))

(define-public ocaml-decoders-yojson
  (package
   (inherit ocaml-decoders)
   (name "ocaml-decoders-yojson")
   (build-system dune-build-system)
   (arguments `(#:tests? #f
		#:package "decoders-yojson"))
   (propagated-inputs (list ocaml-decoders ocaml-yojson))))


(define-public ocaml-monocypher
  (package
   (name "ocaml-monocypher")
   (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://inqlab.net/git/ocaml-monocypher.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pgmlcdg3hkiv97lfw0mza1q1k39hdz98b7xl3ri0prd4zhjm212"))))
   (build-system dune-build-system)
   (arguments '())
   (propagated-inputs
    (list ocaml-ctypes))
   (native-inputs
    (list ocaml-alcotest))
   (home-page "https://inqlab.net/ocaml-monocypher.git")
   (synopsis "OCaml bindings to the Monocypher cryptographic library")
   (description "Monocypher is a cryptographic library. It provides functions
for authenticated encryption, hashing, password hashing and key derivation, key
exchange, and public key signatures.  This library provides OCaml bindings to
Monocypher using Ctypes.")
   (license license:bsd-2)))

(define-public ocaml-eris
  (package
    (name "ocaml-eris")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://inqlab.net/git/ocaml-eris.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kdmgi9mlgd2d0915i4jhjwsgwchzwf8fvqg1xas293lg90iiq3h"))))
    (build-system dune-build-system)
    (arguments `(#:package "eris"
		 #:phases (modify-phases %standard-phases
			    ;; required by Zig
			    (add-before 'build 'set-xdg-cache-home
			      (lambda _
				(setenv "XDG_CACHE_HOME" "/tmp")
				#t)))))
    (native-inputs
     (list zig
	   ocaml-crunch
	   ocaml-decoders-yojson
	   ocaml-bos
	   ocaml-alcotest
	   ocaml-qcheck
	   ocaml-benchmark))
    (propagated-inputs
     (list ocaml-ctypes
	   ocaml-monocypher
	   ocaml-base32
	   ocaml-cborl
	   ocaml-lwt
	   js-of-ocaml))
    (home-page "https://codeberg.org/eris/ocaml-eris")
    (synopsis "OCaml implementation of ERIS")
    (description #f)
    (license license:agpl3+)))

(define-public ocaml-cborl
  (package
    (name "ocaml-cborl")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://inqlab.net/git/ocaml-cborl.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15bw3s82qpwxs9q42sx9rm0qjk1kdy3zm8m283rc907mls0rjx1i"))))
    (build-system dune-build-system)
    (arguments '())
    (propagated-inputs
     (list ocaml-zarith gmp ocaml-fmt))
    (native-inputs
     (list ocaml-alcotest ocaml-qcheck))
    (home-page "https://inqlab.net/git/ocaml-cborl")
    (synopsis "OCaml CBOR library")
    (description 
     "The Concise Binary Object Representation (CBOR), as specified by
RFC 8949, is a binary data serialization format.  CBOR is similar to
JSON but serializes to binary which is smaller and faster to generate
and parse.  This package provides an OCaml implementation of CBOR.")
    (license license:agpl3+)))

(define-public ocaml-rdf
  (package
    (name "ocaml-rdf")
    (version "76bcc116e9eb0bc42c2743f4a57335a803b03fd3")
    (home-page "https://codeberg.org/openEngiadina/ocaml-rdf.git")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
	    (url home-page)
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0y7hn4m0a0v4knygrdan6s4fcjzm6ifq7bywp2ji5kgxbkhaamp8"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (native-inputs
     (list ocaml-alcotest
	   ocaml-qcheck))
    (propagated-inputs
     (list
      ;; core dependencies
      ocaml-uri
      ocaml-fmt
      ocaml-uuidm

      ;; serializations
      ocaml-yojson
      ocaml-xmlm
      ocaml-uunf
      ocaml-sedlex

      ;; CBOR
      ocaml-cborl
      ocaml-base64
      ocaml-base32
      ocaml-uri
      ocaml-z3 z3))
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

(define-public ocaml-note
  (package
    (name "ocaml-note")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://erratique.ch/software/note/releases/note-0.0.2.tbz")
       (sha256
        (base32
         "09la36kpb3hcfyhgkhr1j3b6l3g4jy9b0ps3qbm04pfll1qmfzkg"))))
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
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri
	 (string-append
	  "https://erratique.ch/software/brr/releases/brr-" version
	  ".tbz"))
	(sha256
          (base32
            "0vmh3imq18yybmc3h24wr0z28ql187nwps3kq4n9nsxc7fip3kgl"))))
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

(define-public ocaml-base32
  (package
    (name "ocaml-base32")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://inqlab.net/git/ocaml-base32.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ccalgcnx178dmnb3523gv47xf0hbfry45pg7dix64bn86niq4b1"))))
    (build-system dune-build-system)
    (native-inputs
     (list ocaml-alcotest ocaml-qcheck))
    (home-page "https://inqlab.net/git/ocaml-base32.git")
    (synopsis "Base32 encoding for OCaml")
    (description "Base32 is a binary-to-text encoding that represents
binary data in an ASCII string format by translating it into a
radix-32 representation.  It is specified in RFC 4648.")
    (license license:isc)))

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
    (version "d36b66d0778dfb2cc1c06d657ecd222cdb575acf")
    (home-page "https://codeberg.org/openEngiadina/ocaml-xmppl.git")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url home-page)
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32 "040vhm17r168pc9x4gmvy9bnbaiz0fhgsq0vww7mvd1arf1iryq0"))))
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

(define-public ocaml-leaflet
  (package
   (name "ocaml-leaflet")
   (version "0.1")
   (source
    (origin (method git-fetch)
	    (uri (git-reference
		  (url "https://git.zapashcanon.fr/swrup/leaflet.git")
		  (commit version)))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32 "1g832ab6fgjccq6i6q7kzh3v1bh8caw4nsgv8cx4y9kcxq5xz4cd"))))
   (build-system dune-build-system)
   (propagated-inputs
    (list ocaml-brr js-of-ocaml))
   (home-page "https://git.zapashcanon.fr/swrup/leaflet")
   (synopsis "OCaml bindings for the Leaflet JavaScript library")
   (description #f)
   (license license:bsd-2)))

(define-public ocaml-datalogl
  (package
    (name "ocaml-datalogl")
    (version "8384441213e08a6dcae179ad17f65e7e968427b1")
    (home-page "https://codeberg.org/openEngiadina/ocaml-datalogl")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url home-page)
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32 "03lbsby7562578n1hcmnid3d9y0cgwk9mgsah54d15ah9ibl5rid"))))
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

(define-public ocaml-hmap
  (package
    (name "ocaml-hmap")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri "http://erratique.ch/software/hmap/releases/hmap-0.8.1.tbz")
       (sha256
	(base32 "10xyjy4ab87z7jnghy0wnla9wrmazgyhdwhr4hdmxxdn28dxn03a"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags
       (list "build" "--tests" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs (list ocaml-topkg ocamlbuild opam))
    (home-page "http://erratique.ch/software/hmap")
    (synopsis "Heterogeneous value maps for OCaml")
    (description
     "Hmap provides heterogeneous value maps for OCaml.  These maps bind keys to
values with arbitrary types.  Keys witness the type of the value they are bound
to which allows to add and lookup bindings in a type safe manner.

Hmap has no dependency and is distributed under the ISC license.")
    (license license:isc)))

(define-public ocaml-archi
  (package
    (name "ocaml-archi")
    (version "0.2.0")
    (home-page "https://github.com/anmonteiro/archi")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
	    (url home-page)
	    (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32
	"009rkx7qnwk9yqgmydsa5v8ram79lknzzy0vlw0rqa1i5mdx93gj"))))
    (build-system dune-build-system)
    (arguments `(#:package "archi"))
    (propagated-inputs (list ocaml-hmap))
    (native-inputs (list ocaml-alcotest))
    (synopsis
     "A library for managing the lifecycle of stateful components in OCaml")
    (description
     "Archi is an OCaml library for managing the lifecycle of stateful components and
their dependencies.")
    (license license:bsd-3)))

(define-public ocaml-archi-lwt
  (package
    (inherit ocaml-archi)
    (name "ocaml-archi-lwt")
    (arguments `(#:package "archi-lwt"))
    (propagated-inputs (list ocaml-archi ocaml-lwt))))

(define-public geopub
  (package
    (name "geopub")
    (version "0.0.0")
    (source #f)
    (build-system dune-build-system)
    (arguments '())
    (native-inputs
     (list

      ;; web
      js-of-ocaml
      ocaml-brr
      ocaml-zarith-stubs-js

      ocaml-react
      ocaml-lwt-react

      ;; JS bindings
      ocaml-leaflet

      ;; RDF
      ocaml-rdf
      ocaml-uri
      raptor2

      ;; Database
      ocaml-datalogl
      ocaml-uuseg

      ;; XMPP
      ocaml-xmppl

      ;; Content-addressing
      ocaml-eris

      ;; General utils
      ocaml-archi-lwt
      ocaml-ptime
      ocaml-base32))
    (home-page "https://gitlab.com/openengiadina/geopub")
    (synopsis #f)
    (description #f)
    (license license:agpl3+)))

geopub
