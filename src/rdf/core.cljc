(ns rdf.core
  "RDF in Clojure"
  (:require [clojure.core.logic :as l]))

;; IRI

(defprotocol IIRI
  (iri-value [x] "Returns the IRI value"))

(defprotocol IIRIConvert
  "Protocol for data that can be converted to an iri."
  (-as-iri [x] "Return value as an iri."))

(defrecord IRI [value]
  IIRI
  (iri-value [x] (:value x))

  IComparable
  (-compare [x y] (-compare
                   (:value x)
                   (:value y))))

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
  (literal-canonical [x] "Returns the literal value in canonical form")
  (literal-language [x] "Returns the literal language")
  (literal-datatype [x] "Returns the literal datatype"))

(defprotocol ILiteralConvert
  (-as-literal [x] "Converts value to Literal"))

(defrecord Literal [value language datatype]
  ILiteral
  (literal-value [x] (:value x))
  ;; TODO implement canonical form
  (literal-canonical [x] (:value x))
  (literal-language [x] (:language x))
  (literal-datatype [x] (:datatype x))

  IComparable
  (-compare [x y]
    ;; TODO sort by datatype, then by language and then by value
    (-compare (:value x) (:value y))))

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

         (satisfies? ITripleConvert t) (-as-triple t)

         :else (throw (ex-info "can not cast to triple" t))))

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

;; implement a dummy protocol for nil
(extend-protocol IGraph
  nil
  (graph-match [_ _] '()))

(defn graph? [x]
  (satisfies? IGraph x))

(defprotocol IGraphUpdate
  "Protocol for updating graph"
  (graph-add [x triple] "Add a triple to the graph.")
  (graph-merge [x y] "Merge two graphs.")
  (graph-delete [x triple] "Delete a triple from the graph."))


;; Description
;; A description is a pointer to a specific subject in a graph. This is very useful when talking about a certain subject but still allowing full graph accessability. In particular it allows easier relative queries.

(defrecord Description [subject graph]
  ITripleSeq
  (triple-seq [description]
    (graph-match
     (:graph description)
     (triple (:subject description) (l/lvar) (l/lvar)))))

(defn description
  "Make a new description"
  [subject graph]
  (if (and
       (or (iri? subject)
           (blank-node? subject))
       (graph? graph))
    (->Description subject graph)
    graph))

(defn description? [description]
  (instance? Description description))

(defn description-empty?
  "Returns true if the description has no triples."
  [description]
  (empty? (triple-seq description)))

(defn description-subject
  "Get the subject of a description"
  [description]
  (:subject description))

(defn description-graph
  "Get the underlying graph of a description"
  [description]
  (:graph description))

(defn description-move
  "Make a new description pointing to a new-subject with the same graph"
  [description new-subject]
  (->Description new-subject (description-graph description)))

(defn description-get
  "Get objects for given predicate. This always returns a sequence of objects."
  [description predicate]
  (map triple-object
       (graph-match
        (:graph description)
        (triple (:subject description) predicate (l/lvar)))))

(defn description-get-first
  "Get first object for given predicate. Warning: If there are multiple objects the first is note properly defined."
  [description predicate]
  (first (description-get description predicate)))

(defn description-map-graph
  "Returns a description with f applied on the graph. f must return a graph. If not the output of f will be returned."
  [f desc]
  (let [mapped-graph (f (description-graph desc))]
    (if (graph? mapped-graph)
      (description (description-subject desc) mapped-graph)
      mapped-graph)))

(defn description-add
  "Add a triple to the description"
  [description p o]
  (description-map-graph
   #(graph-add % (triple (description-subject description) p o))
   description))
