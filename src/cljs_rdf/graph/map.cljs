(ns cljs-rdf.graph.map
  "A graph based on hash maps.

  Two indexes are created. One mapping from subject to predicate to object and one from object to predicate to subject. Depending on query the better index is used. "
  (:require [cljs-rdf.core :as rdf]
            [cljs.core.logic :as l]))


;; Index helpers
(defn- assoc-clean
  "If value is non-nil and non-empty replace value of key in map, else dissoc key."
  [map k v]
  (if (< 0 (count v))
    (assoc map k v)
    (dissoc map k)))

(defn- add-to-index
  "add subject predicate object to the mapping from subject to predicate to object"
  [map s p o]
  (merge-with
   ;; if subject already has triple with same predicate
   (fn [predicates new-predicate]
     ;; add object to the set of objects for the predicate
     (merge-with (fn [object-set new-object]
                   (clojure.set/union object-set new-object))
                 predicates new-predicate))
   map
   (hash-map s (hash-map p (hash-set o)))))

(defn- delete-from-index
  "delete from s-p-o mapping"
  [map s p o]
  ;; replace subject to predicate mapping
  (assoc-clean map s
               ;; with predicate to object set mapping
               (assoc-clean (get map s) p
                            ;; with object removed from object-set
                            (disj (get-in map [s p]) o))))

(defn- map-cons
  "cons a to every element of c"
  [c a]
  (map
   ;; cons a to element of c and make sure element of c is a seq.
   #(cons a (if (seq? %) % (list %)))
   c))

;; Match protocol
(defprotocol IMatch
  "Protocol for matching data to a pattern. Pattern may contain logical variables."
  (-match [x pattern] "Match data to pattern and return sequence of matches."))

(extend-protocol IMatch

  nil
  (-match [index pattern]
    '())

  PersistentHashSet
  (-match [index pattern]
    ;; pattern should not be a coll. if it is, take the first value from it.
    (let [pattern (if (coll? pattern) (first pattern) pattern)]
      (if (l/lvar? pattern)
        (seq index)
        (filter (partial = pattern) index))))

  PersistentHashMap
  (-match [index pattern]

    (let [;; the value we match with at this level
          a (first pattern)]

      (if (not (l/lvar? a))

        ;; a is concrete
        (->
         ;; get the sub-data we continue to match on
         (get index a)
         ;; match for the rest of the pattern
         (-match (next pattern))
         ;; add a to list of matches
         (map-cons a))

        ;; a is a logical variable. Iterate over all keys in the map and recursively match on concrete values.
        (mapcat #(-match index (cons % (next pattern)))
                (keys index))))))


(defn- reverse-matches [matches]
  (map reverse matches))

(defn- reify-match [match]
  (let [s (first match)
        p (second match)
        o (nth match 2)]
    (rdf/triple s p o)))

(defn reify-matches [matches]
  (map reify-match matches))

;; Graph
(declare ->Graph)

(defrecord Graph [spo ops]
  rdf/IGraph

  (rdf/graph-add [graph triple]
    (let [s (rdf/triple-subject triple)
          p (rdf/triple-predicate triple)
          o (rdf/triple-object triple)]
      (->Graph
       (add-to-index (:spo graph) s p o)
       (add-to-index (:ops graph) o p s))))

  (rdf/graph-delete [graph triple]
    (let [s (rdf/triple-subject triple)
          p (rdf/triple-predicate triple)
          o (rdf/triple-object triple)]
      (->Graph
       (delete-from-index (:spo graph) s p o)
       (delete-from-index (:ops graph) o p s))))

  (rdf/graph-has [graph triple]
    (let [s (rdf/triple-subject triple)
          p (rdf/triple-predicate triple)
          o (rdf/triple-object triple)]
      (some?
       (get-in (:spo graph) [s p o]))))

  (rdf/graph-match [graph triple]
    (let [s (rdf/triple-subject triple)
          p (rdf/triple-predicate triple)
          o (rdf/triple-object triple)]

      (cond
        ;; if o is concrete use the ops index
        (not (l/lvar? o))
        (-> (:ops graph)
            (-match [o p s])
            ;; don't forget to reverse the matches
            (reverse-matches)
            (reify-matches))

        ;; else use the spo index
        :else
        (-> (:spo graph)
            (-match [s p o])
            (reify-matches)))))

  (rdf/graph-seq [graph]
    (rdf/graph-match graph (rdf/triple (l/lvar) (l/lvar) (l/lvar))))

  (rdf/graph-tripleo [graph q]
    (fn [a]
      (let [q (l/-walk* a q)]
        (cond
          (rdf/triple? q)
          (l/to-stream
           (map #(l/unify a % q)
                (rdf/graph-match graph q)))

          ;; dump all triples
          (l/lvar? q)
          (l/to-stream (map #(l/unify a % q)
                            (rdf/graph-match graph (rdf/triple (l/lvar) (l/lvar) (l/lvar)))))

          ;; will not match with anything that is not a string
          :else (l/to-stream '()))))))

(defn graph [] (->Graph (hash-map) (hash-map)))

;; (-> (graph)
;;     (rdf/graph-add (rdf/triple :a :b :c))
;;     (rdf/graph-add (rdf/triple :a :b :d))
;;     (rdf/graph-match (rdf/triple (l/lvar) (l/lvar) :d))
;;     )
