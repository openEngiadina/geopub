(ns geopub.data.schema
  "Helpers to deal with the schema.org ontology."
  (:require [rdf.core :as rdf]
            [rdf.logic :as rdf-logic]
            [cljs.core.logic :as l]
            [geopub.data.rdf]
            [geopub.ns :refer [schema]])
  (:require-macros [cljs.core.logic :refer [run*]]
                   [rdf.core :refer [defns]]))

(defn- get-name [object]
  (first
   (run* [label]
     (l/conda
      ;; use rdfs label
      ((rdf-logic/description-tripleo object (schema "name") label))

      ;; Fall back to using the subject IRI as label
      ((l/== (rdf/description-subject object) label))))))

(defmethod geopub.data.rdf/description-label-term
  (schema "Event")
  [object & [opts]]
  (get-name object))

(defmethod geopub.data.rdf/description-label-term
  (schema "WebPage")
  [object & [opts]]
  (get-name object))
