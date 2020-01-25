(ns cljs-rdf.core
  (:require [n3]))

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

;; Extend N3 classes to protocols

(def n3-quad-type
  (.-Quad (.-internal (.-DataFactory n3))))

(extend-type n3-quad-type
  Quad
  (quad-subject [x] (.-subject x))
  (quad-predicate [x] (.-subject x))
  (quad-object [x] (.-subject x))
  (quad-graph [x] (.-subject x)))

(def n3-term-type (.-Term (.-internal (.-DataFactory n3))))

(extend-type n3-term-type
  Term
  (term-type [x] (.-termType x))
  (term-value [x] (.-value x)))

(def n3-named-node-type (.-NamedNode (.-internal (.-DataFactory n3))))

(extend-type n3-named-node-type
  NamedNode
  (named-node-iri [x] (.-value x)))

(def n3-blank-node-type (.-BlankNode (.-internal (.-DataFactory n3))))

(extend-type n3-blank-node-type
  BlankNode
  (blank-node-id [x] (.-value x)))

(def n3-literal-type (.-Literal (.-internal (.-DataFactory n3))))

(extend-type n3-literal-type
  Literal
  (literal-value [x] (.-value x))
  (literal-language [x] (.-language x))
  (literal-datatype [x] (.-datatype x)))

(def n3-variable-type (.-Variable (.-internal (.-DataFactory n3))))

(extend-type n3-variable-type
  Variable
  (variable-value [x] (.-value x)))

;; Turtle parser

(defn parse-turtle [input]
  (.parse (new n3/Parser) input))
