(ns rdf.graph.map
  "A in-memory graph based on hash maps.

  Two indexes are created. One mapping from subject to predicate to object and one from object to predicate to subject. Depending on query the better index is used."
  (:require [rdf.core :as rdf]
            [rdf.graph.map.index :refer [index-match index-merge index-remove]]
            [clojure.core.logic :as l]))

;; helpers
(defn- reverse-matches [matches]
  (map reverse matches))

(defn- reify-match [match]
  (let [s (first match)
        p (second match)
        o (nth match 2)]
    (rdf/triple s p o)))

(defn reify-matches [matches]
  (map reify-match matches))

(defn- add-to-index [index a b c]
  (index-merge index
               (hash-map a (hash-map b (hash-set c)))))

(declare ->Graph)

(defrecord Graph [spo ops]
  rdf/IGraph
  (rdf/graph-match [graph triple]
    (let [s (rdf/triple-subject triple)
          p (rdf/triple-predicate triple)
          o (rdf/triple-object triple)]

      (cond
        ;; if o is concrete use the ops index
        (not (l/lvar? o))
        (-> (:ops graph)
            (index-match [o p s])
            ;; don't forget to reverse the matches
            (reverse-matches)
            (reify-matches))

        ;; else use the spo index
        :else
        (-> (:spo graph)
            (index-match [s p o])
            (reify-matches)))))

  rdf/IGraphUpdate
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
       (index-remove (:spo graph) [s p o])
       (index-remove (:ops graph) [o p s]))))

  rdf/ITripleSeq
  (rdf/triple-seq [graph]
    (rdf/graph-match graph
                     (rdf/triple (l/lvar) (l/lvar) (l/lvar)))))

(defn graph [] (->Graph (hash-map) (hash-map)))
