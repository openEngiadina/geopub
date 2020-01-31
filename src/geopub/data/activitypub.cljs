(ns geopub.data.activitypub
  "Helpers to deal with ActivityPub data"
  (:require-macros [cljs.core.logic :refer [run* fresh]])
  (:require [cljs-rdf.core :as rdf]
            [cljs-rdf.graph.map]
            [cljs.core.logic :as l]
            [geopub.ns :refer [as rdfs]]))

(defn get-activity
  "Returns an activty"
  [graph id]
  ;; Create description pointing to the activity
  (rdf/description id
                   ;; with a graph
                   (reduce rdf/graph-add (cljs-rdf.graph.map/graph)
                           ;; containing triples of the activity (three levels deep)
                           (run* [t] (rdf/collecto graph 3 id t)))))

(defn get-activities
  "Returns all activities as a list of RDF descriptions"
  [graph]
  (let
      [;; first find all the activity ids
       activity-ids (run* [id]
                      (fresh [activity-type]

                        ;; Get all possible activity types
                        (rdf/graph-tripleo graph
                                           (rdf/triple activity-type
                                                       (rdfs "subClassOf")
                                                       (as "Activity")))

                        ;; Get all activities
                        (rdf/graph-typeo graph id activity-type))
                      )]

    (map (partial get-activity graph) activity-ids)))
