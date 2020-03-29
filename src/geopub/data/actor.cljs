(ns geopub.data.actor
  "Helpers to deal with Actors/Agents"
  (:require-macros [cljs.core.logic :refer [run fresh]])
  (:require [rdf.core :as rdf]
            [rdf.logic :as rdf-logic]
            [cljs.core.logic :as l]
            [geopub.ns :refer [as foaf]]))

(defmethod geopub.data.rdf/description-label-term
  (as "Person")
  [object & [opts]]
  (first
   (run 1 [label]
     (l/conda
      ;; use preferredUsername
      ((rdf-logic/description-tripleo object (as "preferredUsername") label))

      ;; use preferredUsername
      ((rdf-logic/description-tripleo object (foaf "nick") label))

      ;; use name
      ((rdf-logic/description-tripleo object (as "name") label))

      ;; Fall back to using the subject IRI as label
      ((l/== (rdf/description-subject object) label))))))


