(ns cljs-rdf.dataset.set
  "Implement RDF Dataset using PersistentHashSet"
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
  rdf/IDataset
  (rdf/dataset-add [x quad] (conj x quad))
  (rdf/dataset-delete [x quad] (disj x quad))
  (rdf/dataset-has [x quad] (contains? x quad)))
