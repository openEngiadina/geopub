(ns geopub.db
  (:require [re-frame.core :as re-frame]
            [rdf.core :as rdf]
            [rdf.graph.map]))

(re-frame/reg-event-db
 ::initialize
 (fn [_ _]
   {:current-route nil
    :graph (rdf.graph.map/graph)}))

(re-frame/reg-sub
 ::graph
 (fn [db]
   (:graph db)))

(re-frame/reg-event-db
 ::add-rdf-graph
 (re-frame/path :graph)
 (fn [db [_ graph]]
   (if (rdf/graph? graph)
     (rdf/graph-merge db graph)
     db)))
