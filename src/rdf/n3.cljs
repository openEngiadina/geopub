(ns rdf.n3
  "Bindings to the N3.js library for parsing Turtle"
  (:require [rdf.core :as rdf]
            ["./n3.js" :as n3]
            [cljs.core.async :as async :refer [<! >!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def data-factory (.-DataFactory n3))

;; NamedNode

(def n3-named-node-type
  (.-NamedNode (.-internal data-factory)))

(extend-type n3-named-node-type
  rdf/IIRIConvert
  (-as-iri [x] (rdf/iri (.-value x))))

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

(defprotocol IBlankNodeConvert
  (-as-blank-node [x]))

(extend-type rdf/BlankNode
  IBlankNodeConvert
  (-as-blank-node [x]
    (.blankNode data-factory (rdf/blank-node-id x))))

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

(def n3-quad-type
  (.-Quad (.-internal (.-DataFactory n3))))

(extend-type n3-quad-type
  rdf/ITripleConvert
  (rdf/-as-triple [x]
    (rdf/triple (.-subject x)
                (.-predicate x)
                (.-object x))))

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

;; Parsing

(defn parse [input]
  "Parse RDF/Turtle to a sequence of Triples"
  (map rdf/triple
       ;; convert to Clojure list before mapping
       (js->clj (.parse (new n3/Parser) input))))

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
