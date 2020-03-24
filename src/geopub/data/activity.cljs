(ns geopub.data.activity
  "Helpers to deal with ActivityPub data"
  (:require-macros [cljs.core.logic :refer [run* fresh]])
  (:require [rdf.core :as rdf]
            [rdf.graph.map :as rdf-graph]
            [rdf.logic :as rdf-logic]
            [rdf.description :as rdf-description]
            [rdf.ns :as ns]
            [cljs.core.logic :as l]
            [geopub.ns :refer [as rdfs]]
            [geopub.data.rdf]))

;; Helpers for getting activities from graph

(defn get-activities
  "Returns all activities as a lazy sequence of RDF descriptions"
  [graph]
  (let
      [;; first find all the activity ids
       activity-ids (run* [id]
                      (fresh [activity-type]

                        ;; Get all possible activity types
                        (rdf-logic/graph-tripleo graph
                                           (rdf/triple activity-type
                                                       (rdfs "subClassOf")
                                                       (as "Activity")))

                        ;; Get all activities
                        (rdf-logic/graph-typeo graph id activity-type))
                      )]

    (map #(rdf-description/description % graph) activity-ids)))


;; Helpers for creating an Activity

(defn like
  "Returns an activity to like an object"
  [object]
  (-> (rdf-graph/graph)
      (rdf/graph-add (rdf/triple (rdf/iri "") (ns/rdf :type) (as "Like")))
      (rdf/graph-add (rdf/triple (rdf/iri "") (as :object) object))))

;; Reagent component

(defmethod geopub.data.rdf/description-label-term
  (as "Person")
  [object & [opts]]
  (first
   (run* [label]
     (l/conda
      ;; use preferredUsername
      ((rdf-description/description-tripleo object (as "preferredUsername") label))

      ;; use name
      ((rdf-description/description-tripleo object (as "name") label))

      ;; Fall back to using the subject IRI as label
      ((l/== (rdf-description/description-subject object) label))))))
