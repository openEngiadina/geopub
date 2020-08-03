(ns geopub.db
  (:require [re-frame.core :as re-frame]
            [rdf.core :as rdf]
            [rdf.graph.map]
            [cljs.core.async :as async :refer [<!]])
  (:require-macros [cljs.core.async :refer [go]]))

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


;; a graph in an async channel needs to be handled as a side effect
(re-frame/reg-fx
 ::add-rdf-graph-async
 (fn [chan] (go (re-frame/dispatch [::add-rdf-graph (<! chan)]))))

(re-frame/reg-event-fx
 ::add-rdf-graph
 (re-frame/path :graph)
 (fn [coeffects [_ graph]]
   (cond
     ;; if an RDF graph add to database
     (rdf/graph? graph) {:db (rdf/graph-merge (:db coeffects) graph)}

     ;; if a channel take first element from channel and retry
     (channel? graph) {::add-rdf-graph-async graph}

     :else {})))
