(ns geopub.data.ontology
  "Helpers to deal with ontologies (RDFS and OWL)"
  (:require [rdf.core :as rdf]
            [rdf.logic :as rdf-logic]
            [rdf.ns :refer [rdf rdfs owl]]
            [cljs.core.logic :as l]
            [geopub.data.rdf])
  (:require-macros [cljs.core.logic :refer [run*]]
                   [rdf.core :refer [defns]]))

(defn- get-label [object]
  (first
   (run* [label]
     (l/conda
      ;; use rdfs label
      ((rdf-logic/description-tripleo object (rdfs "label") label))

      ;; Fall back to using the subject IRI as label
      ((l/== (rdf/description-subject object) label))))))

(defmethod geopub.data.rdf/description-label-term
  (rdfs "Class")
  [object & [opts]]
  (get-label object))

(defmethod geopub.data.rdf/description-label-term
  (rdf "Property")
  [object & [opts]]
  (get-label object))

(defmethod geopub.data.rdf/description-label-term
  (owl "Class")
  [object & [opts]]
  (get-label object))
