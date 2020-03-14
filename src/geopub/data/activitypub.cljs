(ns geopub.data.activitypub
  "Helpers to deal with ActivityPub data"
  (:require-macros [cljs.core.logic :refer [run* fresh]])
  (:require [rdf.core :as rdf]
            [rdf.graph.map :as rdf-graph]
            [rdf.logic :as rl]
            [rdf.description :as rd]
            [rdf.ns :as ns]
            [cljs.core.logic :as l]
            [geopub.ns :refer [as rdfs]]))

(defn get-activity
  "Returns an activty"
  [graph id]
  ;; Create description pointing to the activity
  (rd/description id graph))

(defn get-activities
  "Returns all activities as a list of RDF descriptions"
  [graph]
  (let
      [;; first find all the activity ids
       activity-ids (run* [id]
                      (fresh [activity-type]

                        ;; Get all possible activity types
                        (rl/graph-tripleo graph
                                           (rdf/triple activity-type
                                                       (rdfs "subClassOf")
                                                       (as "Activity")))

                        ;; Get all activities
                        (rl/graph-typeo graph id activity-type))
                      )]

    (map (partial get-activity graph) activity-ids)))


(defn like
  "Returns an activity to like an object"
  [object]
  (-> (rdf-graph/graph)
      (rdf/graph-add (rdf/triple (rdf/iri "") (ns/rdf :type) (as "Like")))
      (rdf/graph-add (rdf/triple (rdf/iri "") (as :object) object))))
