(ns rdf.n3
  "Bindings to the N3.js library for parsing Turtle"
  (:require [rdf.core :as rdf]
            ["./n3.js" :as n3]))

(def data-factory (.-DataFactory n3))

;; NamedNode

(def n3-named-node-type
  (.-NamedNode (.-internal data-factory)))

(extend-type n3-named-node-type
  rdf/IIRIConvert
  (-as-iri [x] (rdf/iri (.-value x))))

;; TODO implement I*Convert for all RDF.js types and serialize with N3!
(defprotocol INamedNodeConvert
  "Protocol for types that can be converted to NamedNode"
  (-as-named-node [x]))

(extend-type rdf/IRI
  INamedNodeConvert
  (-as-named-node [x] (.namedNode data-factory (rdf/iri-value x))))

;; BlankNode

(def n3-blank-node-type
  (.-BlankNode (.-internal (.-DataFactory n3))))

(extend-type n3-blank-node-type
  rdf/IBlankNodeConvert
  (-as-blank-node [x] (rdf/blank-node (.-value x))))

;; Literal

(def n3-literal-type
  (.-Literal (.-internal (.-DataFactory n3))))

(extend-type n3-literal-type
  rdf/ILiteralConvert
  (rdf/-as-literal [x]
    (rdf/literal (.-value x)
                 :language (if (not (clojure.string/blank? (.-language x)))
                             (.-language x))
                 :datatype (if (.-datatype x)
                             (rdf/iri (.-datatype x))))))

;; Quad/Triple

(def n3-quad-type
  (.-Quad (.-internal (.-DataFactory n3))))

(extend-type n3-quad-type
  rdf/ITripleConvert
  (rdf/-as-triple [x]
    (rdf/triple (.-subject x)
                (.-predicate x)
                (.-object x))))


;; Parsing

(defn parse [input]
  "Parse RDF/Turtle to a sequence of Triples"
  (map rdf/triple
       ;; convert to Clojure list before mapping
       (js->clj (.parse (new n3/Parser) input))))
