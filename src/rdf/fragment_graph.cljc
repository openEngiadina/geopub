(ns rdf.fragment-graph
  (:require [rdf.core :as rdf]
            [clojure.core.logic :as l]

            [rdf.graph.map.index :refer [index-match index-merge index-remove]]
            [rdf.fragment-graph.csexp :as csexp]
            [rdf.ns :as ns]))

;; Helpers for dealing with URI fragments

(defn iri-base-subject [iri]
  "Returns the base subject of an IRI."
  (cond
    (rdf/iri? iri) (iri-base-subject (rdf/iri-value iri))
    (string? iri) (rdf/iri (first (clojure.string/split iri "#")))))

(defn- get-fragment [iri]
  (cond
    (rdf/iri? iri) (get-fragment (rdf/iri-value iri))
    (string? iri) (or (second (clojure.string/split iri "#")) "")))

(defn- is-fragment? [fg iri]
  (= (iri-base-subject iri) (:base-subject fg)))

(comment
  (is-fragment? (fragment-graph "http://example.com/") "http://example.com/#saf"))

(defn- to-fragment-reference [fg iri]
  (if (is-fragment? fg iri)
    {:f (get-fragment iri)}
    iri))

(defn- expand-fragment-reference [fg fragment-reference]
  (if (:f fragment-reference)
    (rdf/iri (str (rdf/iri-value (:base-subject fg)) "#" (:f fragment-reference)))
    fragment-reference))

(comment
  (expand-fragment-reference (fragment-graph "http://example.com/") {:f "asdf"}))

;; Fragment Graph

(declare ->FragmentGraph)

(defn fragment-graph [base-subject]
  (->FragmentGraph (rdf/iri base-subject) (hash-map) (hash-map)))

(defn base-subject [fg]
  (:base-subject fg))

(defn set-base-subject [fg base-subject]
  (->FragmentGraph
   (rdf/iri base-subject)
   (:statements fg)
   (:fragment-statements fg)))

(defn add-statement [fg predicate object]
  (->FragmentGraph
   (:base-subject fg)
   (index-merge (:statements fg)
                (hash-map (to-fragment-reference fg predicate)
                          (hash-set (to-fragment-reference fg object))))
   (:fragment-statements fg)))

(defn add-fragment-statement [fg fragment predicate object]
  (->FragmentGraph
   (:base-subject fg)
   (:statements fg)
   (index-merge (:fragment-statements fg)
                (hash-map fragment
                          (hash-map (to-fragment-reference fg predicate)
                                    (hash-set (to-fragment-reference fg object)))))))

(defn delete-statement [fg predicate object]
  (->FragmentGraph
   (:base-subject fg)
   (index-remove (:statements fg) (hash-map predicate (hash-set object)))
   (:fragment-statements fg)))

(defn delete-fragment-statement [fg fragment predicate object]
  (->FragmentGraph
   (:base-subject fg)
   (:statements fg)
   (index-remove (:fragment-statements fg)
                 (hash-map fragment (hash-map predicate (hash-set object))))))

(defn- reify-statement-match [fg match]
  (let [s (:base-subject fg)
        p (expand-fragment-reference fg (first match))
        o (expand-fragment-reference fg (second match))]
    (rdf/triple s p o)))

(defn- reify-fragment-statement-match [fg match]
  (let [s (expand-fragment-reference fg {:f (first match)})
        p (expand-fragment-reference fg (second match))
        o (expand-fragment-reference fg (nth match 2))]
    (rdf/triple s p o)))

(defrecord FragmentGraph
    [base-subject statements fragment-statements]

    rdf/IGraphUpdate
    (rdf/graph-add [fg triple]
      (let [s (rdf/triple-subject triple)
            p (rdf/triple-predicate triple)
            o (rdf/triple-object triple)]

        (cond
          (= s (:base-subject fg)) (add-statement fg p o)

          (is-fragment? fg s) (add-fragment-statement fg (get-fragment s) p o)

          :else fg)))

    (rdf/graph-merge [fg-x fg-y]
      (if (= (:base-subject fg-x) (:base-subject fg-y))
        (->FragmentGraph
         (:base-subject fg-x)
         (index-merge (:statements fg-x) (:statements fg-y))
         (index-merge (:fragment-statements fg-x) (:fragment-statements fg-y)))
        fg-x))

    (rdf/graph-delete [fg triple]
      (let [s (rdf/triple-subject triple)
            p (rdf/triple-predicate triple)
            o (rdf/triple-object triple)]

        (cond
          (= s (:base-subject fg)) (delete-statement fg p o)

          (is-fragment? fg s) (delete-fragment-statement fg (get-fragment s) p o)

          :else fg)))

    rdf/IGraph
    (rdf/graph-match [fg triple]
      ;; TODO!!!
      [])

    rdf/ITripleSeq
    (rdf/triple-seq [fg]
      (concat
       (map #(reify-statement-match fg %)
            (index-match (:statements fg) [(l/lvar) (l/lvar)]))
       (map #(reify-fragment-statement-match fg %)
            (index-match (:fragment-statements fg) [(l/lvar) (l/lvar) (l/lvar)])))))

(comment
  (defns ex "http://example.com/")
  (-> (fragment-graph (ex ""))
      (rdf/graph-add (rdf/triple (ex "") (ex "p") (rdf/literal 4)))
      (rdf/graph-add (rdf/triple (ex "") (ex "q") (ex "#a")))
      (rdf/graph-add (rdf/triple (ex "") (ex "#q") (ex "not-a-frag")))
      (rdf/graph-add (rdf/triple (ex "#a") (ex "r") (ex "#b")))
      (rdf/triple-seq)))

;; Transduce

(defn into-fragment-graphs []
  "Returns a stateful transducer that groups a sequence of triples in fragment graphs"
  ;; TODO read up on the thing about early termination (https://clojure.org/reference/transducers#_early_termination)
  ;; I believe I am doing it wrong...
  (fn [xf]
    (let [fragment-graphs (volatile! {})]
      (fn
        ([] (xf))

        ;; finalize and return all fragment graphs
        ([result]
         (let [fgs @fragment-graphs]
           (vreset! fragment-graphs nil)
           (xf (reduce xf result (vals fgs)))))

        ;; add triple to buffered fragment-graphs
        ([result triple]
         (when (rdf/triple? triple)
           (let [base-subject (iri-base-subject
                               (rdf/triple-subject triple))]

             ;; merge fragment-graphs with
             (vswap! fragment-graphs
                     (partial merge-with rdf/graph-merge)

                     ;; a new fragment graph containing only the input triple
                     {base-subject (-> (fragment-graph base-subject)
                                       (rdf/graph-add triple))})))
         ;; don't return anything now
         result)))))

(comment
  (into []
        (into-fragment-graphs)
        [(rdf/triple (ns/ex "") (ns/ex "#foo") (ns/ex "bar"))
         (rdf/triple (ns/ex "") (ns/ex "#foo") (ns/ex "bzr"))
         (rdf/triple (ns/ex "another") (ns/ex "#foo") (ns/ex "bzr"))
         (rdf/triple (ns/ex "another") (ns/ex "#foo2") (ns/ex "bzr"))
         (rdf/triple (ns/ex "another2") (ns/ex "#foo") (ns/ex "bzr"))
         (rdf/triple (ns/ex "another2") (ns/ex "#foo3") (ns/ex "#bzr"))]))

;; Canonical Serialization

(defn- literal->sexp [l]
  (if (= (rdf/literal-datatype l) (ns/rdf "langString"))
    (list "l"
          (rdf/literal-canonical l)
          (rdf/iri-value (rdf/literal-datatype l))
          (rdf/literal-language l))
    (list "l"
          (rdf/literal-canonical l)
          (rdf/iri-value (rdf/literal-datatype l)))))


(comment
  (literal->sexp
   (rdf/literal "hello" :datatype (ns/xsd "string")))

  (literal->sexp
   (rdf/literal "Grüezi"
                :datatype (ns/rdf "langString")
                :language "gsw")))

(defn- term->sexp [term]
  (cond
    (rdf/iri? term) (rdf/iri-value term)
    (rdf/literal? term) (literal->sexp term)
    ;; fragment reference
    (:f term) (list 'f (:f term))))

(defn- statement->sexp [[p o]]
  (list 's (term->sexp p) (term->sexp o)))

(comment
  (statement->sexp
   (list
    (rdf/iri (ns/rdf "test"))
    (rdf/literal 53 :datatype (ns/xsd "integer")))))

(defn- fragment-statement->sexp [[fragment p o]]
  (list 'fs fragment (term->sexp p) (term->sexp o)))

(defn ->csexp [fg]
  (csexp/encode
   (cons 'rdf
         (map (fn [csexp] {:csexp csexp})
              (sort (concat
                     (map (comp csexp/encode statement->sexp)
                          (index-match (:statements fg) [(l/lvar) (l/lvar)]))

                     (map (comp csexp/encode fragment-statement->sexp)
                          (index-match (:fragment-statements fg) [(l/lvar) (l/lvar) (l/lvar)]))))))))

(comment
  (csexp/byte-seq->string
   (->csexp
    (-> (fragment-graph (ns/ex ""))
        (rdf/graph-add (rdf/triple (ns/ex "") (ns/ex "p") (rdf/literal 4 :datatype (ns/xsd "integer"))))
        (rdf/graph-add (rdf/triple (ns/ex "") (ns/ex "q") (ns/ex "#a")))
        (rdf/graph-add (rdf/triple (ns/ex "") (ns/ex "#q") (ns/ex "not-a-frag")))
        (rdf/graph-add (rdf/triple (ns/ex "#a") (ns/ex "r") (ns/ex "#b")))))))

