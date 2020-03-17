(ns rdf.core
  "RDF in Clojure")

(declare iri)

(defmacro defns
  "Macro for defining a custom new namespace"
  [prefix namespace]
  (let
      [relative (gensym "relative-")]
      `(defn ~prefix
         [~relative]
         (iri
          (str ~namespace (name ~relative))))))


;; IRI

(defprotocol IIRI
  (iri-value [x] "Returns the IRI value"))

(defprotocol IIRIConvert
  "Protocol for data that can be converted to an iri."
  (-as-iri [x] "Return value as an iri."))

(defrecord IRI [value]
  IIRI
  (iri-value [x] (:value x)))

(defn iri? [x] (instance? IRI x))

(defn iri
  "Returns an iri"
  [v]
  (cond 
    ;; value is already an iri
    (instance? IRI v) v

    ;; value can be converted to an iri
    (satisfies? IIRIConvert v) (-as-iri v)

    ;; else just pack in an IRI
    :else (->IRI v)))

;; Literal
(defprotocol ILiteral
  (literal-value [x] "Returns the literal value")
  (literal-language [x] "Returns the literal language")
  (literal-datatype [x] "Returns the literal datatype"))

(defprotocol ILiteralConvert
  (-as-literal [x] "Converts value to Literal"))

(defrecord Literal [value language datatype]
  ILiteral
  (literal-value [x] (:value x))
  (literal-language [x] (:language x))
  (literal-datatype [x] (:datatype x)))

(defn literal? [x] (instance? Literal x))

(defn literal
  "Returns a literal with optional language and datatype."
  ([value & {:keys [language datatype]}]
   (cond
     ;; already a literal, nothing to do
     (instance? Literal value) value

     ;; value can be converted to a literal
     (satisfies? ILiteralConvert value) (-as-literal value)

     ;; return a new Literal
     :else (->Literal value language datatype))))

;; Blank Node

(defprotocol IBlankNode
  (blank-node-id [x] "Returns the blank node id"))

(defprotocol IBlankNodeConvert
  (-as-blank-node [x] "Converts value to BlankNode"))

(defrecord BlankNode [id]
  IBlankNode
  (blank-node-id [x] (:id x)))

(defn blank-node? [x]
  (instance? BlankNode x))

(defn blank-node
  "Returns a blank node. If no id is suplied a new (and unique) id will be generated."
  ([] (->BlankNode (gensym)))
  ([v]
   (cond
     (instance? BlankNode v) v
     (satisfies? IBlankNodeConvert v) (-as-blank-node v)
     :else (->BlankNode v))))


;; Triple
(defprotocol ITriple
  (triple-subject [x] "Returns the triple subject")
  (triple-predicate [x] "Returns the triple predicate")
  (triple-object [x] "Returns the triple object"))

(defprotocol ITripleConvert
  (-as-triple [x] "Converts value to Triple"))

(defprotocol ITripleSeq
  "Protocol for converting anything to a sequence of triples"
  (triple-seq [x]))

(extend-type default
  ITripleSeq
  (triple-seq [x]
    (cond
      (seqable? x) (seq x)
      (satisfies? ITriple x) (list x))))

(defrecord Triple [subject predicate object]
  ITriple
  (triple-subject [x] (:subject x))
  (triple-predicate [x] (:predicate x))
  (triple-object [x] (:object x))

  ITripleSeq
  (triple-seq [x] (list x)))

(defn triple? [t]
  (instance? Triple t))

(defn- iri-like? [s]
  (or (iri? s) (satisfies? IIRIConvert s)))

(defn- blank-node-like? [s]
  (or (blank-node? s) (satisfies? IBlankNodeConvert s)))

(defn- literal-like? [s]
  (or (literal? s) (satisfies? ILiteralConvert s)))

(defn triple
  "Returns a Triple"
  ([t] (cond
         (instance? Triple t) t

         (satisfies? ITripleConvert t) (-as-triple t)))

  ([s p o] (->Triple
            ;; attempt to cast subject
            (cond
              ;; to IRI
              (iri-like? s) (iri s)
              ;; or BlankNode
              (blank-node-like? s) (blank-node s)
              ;; else just use as is
              :else s)

            ;; attempt to cast predicate
            (cond
              ;;  to IRI
              (iri-like? p) (iri p)
              ;; else just use as is
              :else p)

            ;; attempt to cast object
            (cond
              ;; to IRI
              (iri-like? o) (iri o)
              ;; or BlankNode
              (blank-node-like? o) (blank-node o)
              ;; or Literal
              (literal-like? o) (literal o)
              ;; else just use as is
              :else o))))

;; TODO quads

;; Graph Protocols

(defprotocol IGraph
  "Protocol for accessing a graph"
  (graph-match [x triple] "Returns sequence of triples matching query."))

(defn graph? [x]
  (satisfies? IGraph x))

(defprotocol IGraphUpdate
  "Protocol for updating graph"
  (graph-add [x triple] "Add a triple to the graph.")
  (graph-merge [x y] "Merge two graphs.")
  (graph-delete [x triple] "Delete a triple from the graph."))

