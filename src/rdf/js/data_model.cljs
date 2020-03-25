(ns rdf.js.data-model
  "Bindings to the RDF/JS Data model specification (http://rdf.js.org/data-model-spec/).

  Note that per RDFJS data model specification the types are strinlgy typed using the `termType` attribute. We do not want to do stringly typing (it doesn't fit into Clojure's polymorphism). We use JavaScript prototypes magic.

  We implement bindings to the explicit classes defined in the @rdfjs/data-model library. Almost everything just works with this. Except N3.js.

  N3.js implments it's own types. We also implement bindings to N3.js types here. "
  (:require [rdf.core :as rdf]
            ["n3" :as n3]
            ["@rdfjs/data-model" :as rdf-js]
            [goog.date]))


;; NamedNode (IRI)
 
(def named-node-type (type (.namedNode rdf-js)))

(extend-type named-node-type
    rdf/IIRIConvert
    (-as-iri [x] (rdf/iri (.-value x))))

(def n3-named-node-type
  (.-NamedNode (.-internal (.-DataFactory n3))))

(extend-type n3-named-node-type
  rdf/IIRIConvert
  (-as-iri [x] (rdf/iri (.-value x))))

;; BlankNode

(def blank-node-type (type (.blankNode rdf-js)))

(extend-type blank-node-type
  rdf/IBlankNodeConvert
  (-as-blank-node [x] (rdf/blank-node (.-value x))))

(def n3-blank-node-type
  (.-BlankNode (.-internal (.-DataFactory n3))))

(extend-type n3-blank-node-type
  rdf/IBlankNodeConvert
  (-as-blank-node [x] (rdf/blank-node (.-value x))))

;; Literal

(defn- as-literal [x]
  (rdf/literal
   ;; NOTE An attempt was made to cast value to proper types (from string). I.e. cast values with datatype xsd:dateTime to js/Date. This seems to be very opinionated and always incomplete. Better leave value in lexical form and have upper layers cast value to whatever they want.
   (.-value x)
   :language (if (not (clojure.string/blank? (.-language x)))
               (.-language x))
   :datatype (if (.-datatype x)
               (rdf/iri (.-datatype x)))))

(def literal-type (type (.literal rdf-js)))

(extend-type literal-type
  rdf/ILiteralConvert
  (rdf/-as-literal [x] (as-literal x))
  )

(def n3-literal-type
  (.-Literal (.-internal (.-DataFactory n3))))

(extend-type n3-literal-type
  rdf/ILiteralConvert
  (rdf/-as-literal [x] (as-literal x)))

;; Quad (Triple)

(def quad-type (type (.quad rdf-js)))

(extend-type quad-type
  rdf/ITripleConvert
  (rdf/-as-triple [x]
    (rdf/triple (.-subject x)
                (.-predicate x)
                (.-object x))))

(def n3-quad-type
  (.-Quad (.-internal (.-DataFactory n3))))

(extend-type n3-quad-type
  rdf/ITripleConvert
  (rdf/-as-triple [x]
    (rdf/triple (.-subject x)
                (.-predicate x)
                (.-object x))))
