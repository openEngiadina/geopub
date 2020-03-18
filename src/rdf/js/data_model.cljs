(ns rdf.js.data-model
  "Bindings to the RDF/JS Data model specification (http://rdf.js.org/data-model-spec/).

  Note that the bindings use the explicit class defined in the @rdfjs/data-model library instead of checking the `termType` attribute of values. This means that this binding will only work with libraries that use the @rdfjs/data-model library (i.e. not N3.js)."

  (:require [rdf.core :as rdf]
            ["@rdfjs/data-model" :as rdf-js]))


;; Note: I understand way too little about the JavaScript prototype thing. But this works...somehow (?!?)

;; NamedNode (IRI)
 
(def named-node-type (type (.namedNode rdf-js)))

(extend-type named-node-type
    rdf/IIRIConvert
    (-as-iri [x] (rdf/iri (.-value x))))

;; BlankNode

(def blank-node-type (type (.blankNode rdf-js)))

(extend-type blank-node-type
  rdf/IBlankNodeConvert
  (-as-blank-node [x] (rdf/blank-node (.-value x))))

;; Literal

(def literal-type (type (.literal rdf-js)))

(extend-type literal-type
  rdf/ILiteralConvert
  (rdf/-as-literal [x]
    (rdf/literal (.-value x)
                 :language (if (not (clojure.string/blank? (.-language x)))
                             (.-language x))
                 :datatype (if (.-datatype x)
                             (rdf/iri (.-datatype x))))))

;; Quad (Triple)

(def quad-type (type (.quad rdf-js)))

(extend-type quad-type
  rdf/ITripleConvert
  (rdf/-as-triple [x]
    (rdf/triple (.-subject x)
                (.-predicate x)
                (.-object x))))
