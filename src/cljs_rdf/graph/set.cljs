(ns cljs-rdf.graph.set
  "Implement RDF Graph using PersistentHashSet"
  (:require [cljs-rdf.core :as rdf]
            [cljs.core.logic :as l]))


(extend-type PersistentHashSet
  rdf/IGraph
  (rdf/graph-add [x triple] (conj x triple))
  (rdf/graph-delete [x triple] (disj x triple))
  (rdf/graph-has [x triple] (contains? x triple))

  (rdf/graph-tripleo [x q]
    (fn [s]
      (l/to-stream
       ;; TODO plenty of room for optimizing. Currently just treats graph as a seq.
       (map #(l/unify s % q) x)))))



