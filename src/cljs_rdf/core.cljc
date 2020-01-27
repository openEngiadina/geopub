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

(ns cljs-rdf.core
  "RDF in ClojureScript")

(defmacro defns
  "Create a new namespace"
  [prefix namespace]
  (let
      [relative (gensym "relative-")]
    `(defn ~prefix
       [~relative]
       (str ~namespace (name ~relative)))))

;; RDF/JS Data model specification (http://rdf.js.org/data-model-spec/)

;; TODO: implement equality

(defprotocol Quad
  (quad-subject [x])
  (quad-predicate [x])
  (quad-object [x])
  (quad-graph [x]))

(defprotocol Term
  (term-type [x])
  (term-value [x]))

(defprotocol NamedNode
  (named-node-iri [x]))

(defprotocol BlankNode
  (blank-node-id [x]))

(defprotocol Literal
  (literal-value [x])
  (literal-language [x])
  (literal-datatype [x]))

(defprotocol Variable
  (variable-value [x]))


;; Implement protocol for basic ClojureScript types

(extend-type js/String
  NamedNode
  (named-node-iri [x] x)

  Literal
  (literal-value [x] x)
  (literal-language [x] nil)
  (literal-datatype [x] nil))

