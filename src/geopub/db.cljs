(ns geopub.db
  (:require [re-frame.core :as re-frame]
            [rdf.core :as rdf]
            [rdf.graph.map]
            [rdf.fragment-graph]
            [cljs.core.async :as async :refer [<!]])
  (:require-macros [cljs.core.async :refer [go-loop]]))

(re-frame/reg-event-db
 ::initialize
 (fn [_ _]
   {:current-route nil
    :graph (rdf.graph.map/graph)}))

(re-frame/reg-sub
 ::graph
 (fn [db]
   (:graph db)))

(defn- channel?
  [x]
  (satisfies? cljs.core.async.impl.protocols/Channel x))

(re-frame/reg-event-fx
 ::add-rdf-graph
 (re-frame/path :graph)
 (fn [coeffects [_ graph]]
   (cond
     ;; if an RDF graph add to database
     (rdf/graph? graph) {:db (rdf/graph-merge (:db coeffects) graph)}

     ;; handle async/channel of graphs
     (channel? graph)
     {:dispatch-async (async/map #(conj [::add-rdf-graph] %) [graph])}

     :else {})))
