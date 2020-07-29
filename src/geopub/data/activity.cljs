(ns geopub.data.activity
  "Helpers to deal with ActivityPub data"
  (:require-macros [cljs.core.logic :refer [run* run fresh]])
  (:require [rdf.core :as rdf]
            [rdf.graph.map :as rdf-graph]
            [rdf.logic :as rdf-logic]
            [rdf.ns :refer [rdf rdfs]]
            [geopub.ns :refer [as dc]]
            [cljs.core.logic :as l]))

;; Helpers for getting activities from graph

;; TODO: move this to some other place and generalize to other properties other than as:published (such as dc:createdAt)
(defn- sort-by-date [descriptions]
  (sort-by
   ;; get the as:published property and cast to js/Date
   (fn [description]
     (->> (rdf/description-get description (as "published"))
          (first)
          (rdf/literal-value)
          (new js/Date)))
   ;; reverse the sort order
   (comp - compare)
   descriptions))

(defn- related-activityo [graph related-to activity]
  (l/conda
   ;; there is a triple with any property with activity as subject and related-to as object
   [(fresh [p] (rdf-logic/graph-tripleo graph activity p related-to))]
   [l/s# l/u#]))

(defn get-related-activities
  [graph related-to]
  (->> (run* [activity]
         (rdf-logic/graph-typeo graph activity (as "Activity"))
         (related-activityo graph related-to activity))
       (map #(rdf/description % graph))
       (sort-by-date)))

(defn get-activities
  "Returns all activities as a lazy sequence of RDF descriptions"
  [graph]
  (->> (run* [id] (rdf-logic/graph-typeo graph id (as "Activity")))
       (map #(rdf/description % graph))
       (sort-by-date)))

;; Helpers for creating an Activity

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
