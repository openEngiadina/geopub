(ns geopub.data.activity
  "Helpers to deal with ActivityPub data"
  (:require-macros [cljs.core.logic :refer [run* run fresh]])
  (:require [rdf.core :as rdf]
            [rdf.graph.map :as rdf-graph]
            [rdf.logic :as rdf-logic]
            [rdf.ns :refer [rdf rdfs]]

            [geopub.ns :refer [as dc]]
            [geopub.rdf]

            [cljs.core.logic :as l]))

(defn like
  "Returns an activity to like an object"
  [object]
  (-> (rdf/description (rdf/iri "") (rdf-graph/graph))
      (rdf/description-add (rdf "type") (as "Like"))
      (rdf/description-add (as "object") object)
      (rdf/description-add (as "to") (as "Public"))))

;; Reagent component

(defn creator-label-term [object & [opts]]
  (first
   (run 1 [label]
     (l/conda
      ;; use name
      ((rdf-logic/description-tripleo object (as "attributedTo") label))

      ;; use dc:title
      ((rdf-logic/description-tripleo object (dc "creator") label))

      ;; Fall back to using the subject IRI as label
      ((l/== (rdf/description-subject object) label))))))
