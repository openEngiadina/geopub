(ns geopub.data.ontology
  "Helpers to deal with ontologies (RDFS and OWL)"
  (:require [rdf.core :as rdf]
            [rdf.logic :as rdf-logic]
            [rdf.description :as rdf-description]
            [cljs.core.logic :as l]
            [geopub.data.rdf])
  (:require-macros [cljs.core.logic :refer [run*]]
                   [rdf.core :refer [defns]]))

(defns rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(defns rdfs "http://www.w3.org/2000/01/rdf-schema#")
(defns owl "http://www.w3.org/2002/07/owl#")

(defn- get-label [object]
  (first
   (run* [label]
     (l/conda
      ;; use rdfs label
      ((rdf-description/description-tripleo object (rdfs "label") label))

      ;; Fall back to using the subject IRI as label
      ((l/== (rdf-description/description-subject object) label))))))

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
