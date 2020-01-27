;; Copyright © 2020 pukkamustard <pukkamustard@posteo.net>
;;
;; This file is part of GeoPub.
;;
;; GeoPub is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GeoPub is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GeoPub.  If not, see <https://www.gnu.org/licenses/>.

(ns cljs-rdf.n3
  "Bindings to the N3.js library"
  (:require [cljs-rdf.core :as rdf]
            [n3]))

;; Extend N3 classes to protocols
(def n3-quad-type
  (.-Quad (.-internal (.-DataFactory n3))))

(extend-type n3-quad-type
  rdf/IQuad
  (rdf/quad-subject [x] (.-subject x))
  (rdf/quad-predicate [x] (.-predicate x))
  (rdf/quad-object [x] (.-object x))
  (rdf/quad-graph [x] (.-graph x))

  IPrintWithWriter
  (-pr-writer [o w _]
    (write-all w
               "#Quad {"
               "subject: " (prn-str (rdf/quad-subject o))
               ", predicate: " (prn-str (rdf/quad-predicate o))
               ", object: " (prn-str (rdf/quad-object o))
               ", graph: " (prn-str (rdf/quad-graph o))
               "}"
               )))


(def n3-term-type (.-Term (.-internal (.-DataFactory n3))))

(extend-type n3-term-type
  rdf/ITerm
  (rdf/term-type [x] (.-termType x))
  (rdf/term-value [x] (.-value x)))

(def n3-named-node-type (.-NamedNode (.-internal (.-DataFactory n3))))

(extend-type n3-named-node-type
  rdf/INamedNode
  (rdf/named-node-iri [x] (.-value x))

  IPrintWithWriter
  (-pr-writer [o w _] (write-all w "<" (rdf/named-node-iri o) ">")))

(def n3-blank-node-type (.-BlankNode (.-internal (.-DataFactory n3))))

(extend-type n3-blank-node-type
  rdf/IBlankNode
  (rdf/blank-node-id [x] (.-value x))

  IPrintWithWriter
  (-pr-writer [o w _] (write-all w "_:" (prn-str (rdf/blank-node-id o))))
  )

(def n3-literal-type (.-Literal (.-internal (.-DataFactory n3))))

(extend-type n3-literal-type
  rdf/ILiteral
  (rdf/literal-value [x] (.-value x))
  (rdf/literal-language [x] (.-language x))
  (rdf/literal-datatype [x] (.-datatype x))

  IPrintWithWriter
  ;; TODO also print datatype and language
  (-pr-writer [o w _] (write-all w "\"" (pr-str (rdf/literal-value o)) "\"")))

(def n3-variable-type (.-Variable (.-internal (.-DataFactory n3))))

(extend-type n3-variable-type
  rdf/IVariable
  (rdf/variable-value [x] (.-value x)))

;; Turtle parser

(defn parse [input]
  "Parse an RDF document to quads"
  (js->clj (.parse (new n3/Parser) input)))
