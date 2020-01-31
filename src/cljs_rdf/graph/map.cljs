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

(defn seq-match [index a]
  ;; TODO return a lazy seq
  (map list
       (if (l/lvar? a)
         (seq index)
         (filter (partial = a) index))))

(defn- map-cons
  "cons a to every element of c"
  [c a]
  (map (partial cons a) c))

(defn- reverse-matches [matches]
  (map reverse matches))

(defn- reify-match [match]
  (let [s (first match)
        p (second match)
        o (nth match 2)]
    (rdf/triple s p o)))

(defn reify-matches [matches]
  (map reify-match matches))

(defn- index2-match
  [index a b]


  (if (not (l/lvar? a))

    ;; a is a concrete value
    (->
     ;; get the index1 for a
     (get index a)
     ;; match for b
     (seq-match b)
     ;; add a to the returned matches
     (map-cons a)
     )

    ;; iterate over all possible as and recursively match with concrete value
    (mapcat #(index2-match index % b)
            (keys index))))

(defn- index3-match
  [index a b c]
  (if (not (l/lvar? a))

    ;; a is a concrete value
    (->
     ;; get the index for a
     (get index a)
     ;; match for b c
     (index2-match b c)
     ;; add a to the returned matches
     (map-cons a))

    ;; iterate over all possible as and recursively match with concrete value
    (mapcat #(index3-match index % b c)
            (keys index))))

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
            (index3-match o p s)
            ;; don't forget to reverse the matches
            (reverse-matches)
            (reify-matches))

        ;; else use the spo index
        :else
        (-> (:spo graph)
            (index3-match s p o)
            (reify-matches)))))

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
          :else (l/to-stream '())))))
  )

(defn graph [] (->Graph {} {}))

;; (-> (graph)
;;     (rdf/graph-add (rdf/triple :a :b :c))
;;     (rdf/graph-add (rdf/triple :a :b :d))
;;     (rdf/graph-match (rdf/triple (l/lvar) (l/lvar) (l/lvar)))
;;     )
