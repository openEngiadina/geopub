(ns rdf.fragment-graph
  (:require [rdf.core :as rdf]
            [clojure.core.logic :as l]
            [rdf.graph.map.index :refer [index-match index-merge index-remove]])
  (:require-macros [rdf.core :refer [defns]]))

;; Helpers for dealing with URI fragments

(defn- get-fragment [iri]
  (cond
    (rdf/iri? iri) (get-fragment (rdf/iri-value iri))
    (string? iri) (.getFragment (new goog.Uri iri))))

(defn- is-fragment? [fg iri]
  (let [iri (if (rdf/iri? iri) (rdf/iri-value iri) iri)]
    (= (str (.setFragment (new goog.Uri iri) ""))
       (str (new goog.Uri (rdf/iri-value (:base-subject fg)))))))

(comment
  (is-fragment? (fragment-graph "http://example.com/") "http://example.com/#saf"))

(defn- to-fragment-reference [fg iri]
  (if (is-fragment? fg iri)
    {:f (get-fragment iri)}
    iri))

(defn- expand-fragment-reference [fg fragment-reference]
  (if (:f fragment-reference)
    (-> (new goog.Uri (rdf/iri-value (:base-subject fg)))
        (.setFragment (:f fragment-reference))
        (str)
        (rdf/iri))
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
      (if (= (base-subject fg-x) (base-subject fg-y))
        (->FragmentGraph
         (base-subject fg-x)
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
