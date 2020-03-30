(ns geopub.data.activity
  "Helpers to deal with ActivityPub data"
  (:require-macros [cljs.core.logic :refer [run* run fresh]])
  (:require [rdf.core :as rdf]
            [rdf.graph.map :as rdf-graph]
            [rdf.logic :as rdf-logic]
            [rdf.ns :refer [rdf rdfs]]
            [geopub.ns :refer [as dc]]
            [cljs.core.logic :as l]
            [geopub.data.rdf]))

;; Helpers for getting activities from graph

(defn get-activities
  "Returns all activities as a lazy sequence of RDF descriptions"
  [graph]
  (let
      [activity-ids (run* [id]
                      (fresh [activity-type]

                        ;; Get all possible activity types
                        (rdf-logic/graph-tripleo graph
                                                 (rdf/triple activity-type
                                                             (rdfs "subClassOf")
                                                             (as "Activity")))

                        ;; Get all activities
                        (rdf-logic/graph-typeo graph id activity-type)))]

    (->> activity-ids
         (map #(rdf/description % graph))
         (sort-by
          ;; get the as:published property and cast to js/Date
          (fn [description]
            (->> (rdf/description-get description (as "published"))
                 (first)
                 (rdf/literal-value)
                 (new js/Date)))
          ;; reverse the sort order
          (comp - compare)))))

;; Helpers for creating an Activity

(defn like
  "Returns an activity to like an object"
  [object]
  (-> (rdf-graph/graph)
      (rdf/graph-add (rdf/triple (rdf/iri "") (rdf :type) (as "Like")))
      (rdf/graph-add (rdf/triple (rdf/iri "") (as :object) object))))

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
