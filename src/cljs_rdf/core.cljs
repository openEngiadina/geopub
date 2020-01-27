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
  "RDF in ClojureScript"
  (:require-macros [cljs-rdf.core :refer [defns]]))

;; Commonly used namespaces

(defns rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(defns xsd "http://www.w3.org/2001/XMLSchema#")

;; RDF/JS Data model specification (http://rdf.js.org/data-model-spec/)

;; TODO: implement equality

(defprotocol IQuad
  (quad-subject [x])
  (quad-predicate [x])
  (quad-object [x])
  (quad-graph [x]))

(defn quad? [x]
  (satisfies? IQuad x))

(defprotocol ITerm
  (term-type [x])
  (term-value [x]))

(defn term? [x]
  (satisfies? ITerm x))

(defprotocol INamedNode
  (named-node-iri [x]))

(defn named-node? [x]
  (satisfies? INamedNode x))

(defprotocol IBlankNode
  (blank-node-id [x]))

(defn blank-node? [x]
  (satisfies? IBlankNode x))

(defprotocol ILiteral
  (literal-value [x])
  (literal-language [x])
  (literal-datatype [x]))

(defn literal? [x]
  (satisfies? ILiteral x))

(defprotocol IVariable
  (variable-value [x]))

(defn variable? [x]
  (satisfies? IVariable x))

;; Dataset (inspired by RDF/JS Dataset specification, https://rdf.js.org/dataset-spec/)

(defprotocol IDataset
  (dataset-add [x quad] "Add a quad to the dataset.")
  (dataset-delete [x quad] "Remove quad from the dataset.")
  (dataset-has [x quad] "Returns true if quad is in dataset, false if not."))

(defn dataset? [x]
  (satisfies? IDataset x))

;; Implement protocol for basic types

(extend-type js/String
  INamedNode
  (named-node-iri [x] x)

  ILiteral
  (literal-value [x] x)
  (literal-language [x] nil)
  (literal-datatype [x] (xsd "string")))

(extend-type js/Number
  ILiteral
  (literal-value [x] x)
  (literal-language [x] nil)
  (literal-datatype [x]
    (do
      (cond
        (integer? (.valueOf x)) (xsd "integer")
        (double? (.valueOf x)) (xsd "double")))))

;; Implementation of data model using records

(defrecord Quad [subject predicate object graph]
  IQuad
  (quad-subject [q] (:subject q))
  (quad-predicate [q] (:predicate q))
  (quad-object [q] (:object q))
  (quad-graph [q] (:graph q)))

(defrecord BlankNode [id]
  IBlankNode
  (blank-node-id [b] (:id b)))

(defrecord Literal [value language datatype]
  ILiteral
  (literal-value [l] (:value l))
  (literal-language [l] (:language l))
  (literal-datatype [l] (:language l)))
