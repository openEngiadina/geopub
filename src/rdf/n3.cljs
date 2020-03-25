(ns rdf.n3
  "Bindings to the N3.js library for parsing Turtle"
  (:require [rdf.core :as rdf]
            ["n3" :as n3]
            [cljs.core.async :as async :refer [<! >!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def data-factory (.-DataFactory n3))

;; NamedNode

(defprotocol INamedNodeConvert
  "Protocol for types that can be converted to NamedNode"
  (-as-named-node [x]))

(extend-type rdf/IRI
  INamedNodeConvert
  (-as-named-node [x] (.namedNode data-factory (rdf/iri-value x))))

;; BlankNode

(defprotocol IBlankNodeConvert
  (-as-blank-node [x]))

(extend-type rdf/BlankNode
  IBlankNodeConvert
  (-as-blank-node [x]
    (.blankNode data-factory (rdf/blank-node-id x))))

;; Literal

(defprotocol ILiteralConvert
  (-as-literal [x]))

(extend-type rdf/Literal
  ILiteralConvert
  (-as-literal [x]
    (.literal data-factory
              (rdf/literal-value x)
              (cond
                (rdf/literal-language x) (rdf/literal-language x)

                (rdf/literal-datatype x) (-as-named-node
                                          (rdf/literal-datatype x))

                :else nil))))

;; Quad/Triple

(defprotocol IQuadConvert
  (-as-quad [x]))

(extend-type rdf/Triple
  IQuadConvert
  (-as-quad [x]
    (let [s (rdf/triple-subject x)
          p (rdf/triple-predicate x)
          o (rdf/triple-object x)]
      (.quad data-factory
             ;; subject
             (cond
               (implements? INamedNodeConvert s) (-as-named-node s)
               (implements? IBlankNodeConvert s) (-as-blank-node s)
               :else s)

             ;; predicate
             (cond
               (implements? INamedNodeConvert p) (-as-named-node p)
               :else p)

             (cond
               (implements? INamedNodeConvert o) (-as-named-node o)
               (implements? IBlankNodeConvert o) (-as-blank-node o)
               (implements? ILiteralConvert o) (-as-literal o))))))

;; Encoding
 
(defn- await-cb*
  "Calls the function given with supplied arguments and a callback function as last argument. Returns a channel that will hold the values passed to the callback function."
  [f & args]
  (let [c (async/chan)]
    (apply f (concat args [(fn [& results] (go (>! c results)
                                               (async/close! c)))]))
    c))

(defn- writer-add-triple [writer triple]
  (.addQuad writer (-as-quad triple))
  writer)

(defn- writer-end [writer cb]
  (.end writer cb))

(defn- handle-encode-error
  "with handle we mean ignore..."
  [c] (async/pipe c (async/chan 1 (map second))))

(defn encode
  "Encode a triple or sequence of triples as RDF/Turtle"
  [input]
  (let [triples (cond
                  (implements? rdf/ITripleSeq input) (rdf/triple-seq input)
                  (implements? ISeq input) (seq input)
                  :else (list input))]
    (->> triples
         ;; add triples to the writer
         (reduce writer-add-triple (new n3/Writer))
         ;; end writer with callback function
         (await-cb* writer-end)
         ;; handle error
         (handle-encode-error)
         )))
