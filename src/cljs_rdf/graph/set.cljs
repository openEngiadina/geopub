(ns cljs-rdf.graph.set
  "Implement RDF Graph using PersistentHashSet"
  (:require [cljs-rdf.core :as rdf]
            [cljs.core.logic :as l]))



(extend-type PersistentHashSet
  rdf/IGraph
  (rdf/graph-add [graph triple] (conj graph triple))
  (rdf/graph-delete [graph triple] (disj graph triple))
  (rdf/graph-has [graph triple] (contains? graph triple))

  (rdf/graph-match [graph triple]
    ;; TODO nested matching. Currently it is not possible to get all triples that have a BlankNode as subject (while not specifying the id of the BlankNode)
    (let [s (rdf/triple-subject triple)
          p (rdf/triple-predicate triple)
          o (rdf/triple-object triple)]

      (filter
       (fn [tg]
         (let [sg (rdf/triple-subject tg)
               pg (rdf/triple-predicate tg)
               og (rdf/triple-object tg)]
           (and
            (or (l/lvar? s) (= s sg))
            (or (l/lvar? p) (= p pg))
            (or (l/lvar? o) (= o og)))))
       graph)))

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
          (l/to-stream (map #(l/unify a % q) graph))

          ;; will not match with anything that is not a string
          :else (l/to-stream '()))))))

