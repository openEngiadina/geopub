(ns cljs-rdf.graph.set
  "Implement RDF Graph using PersistentHashSet"
  (:require [cljs-rdf.core :as rdf]))


;; (defn- cast [quad]
;;   "cast to a native quad"
;;   (rdf/->Quad
;;    (rdf/quad-subject quad)
;;    (rdf/quad-predicate quad)
;;    (rdf/quad-object quad)
;;    (rdf/quad-graph quad)))

;; TODO: This implementation of dataset allows adding the identical quad mutltiple times if the underlying implementation of the Quad is different. Fixing this requires casting it to a "canonical" representation.

(extend-type PersistentHashSet
  rdf/IGraph
  (rdf/graph-add [x quad] (conj x quad))
  (rdf/graph-delete [x quad] (disj x quad))
  (rdf/graph-has [x quad] (contains? x quad)))

