(ns geopub.rdf.ontology
  "load bundled ontologies"
  (:require [re-frame.core :as re-frame]
            [geopub.db :as db]))

(re-frame/reg-sub
 ::loaded?
 (fn [db _]
   (:ontologies-loaded? db)))

(re-frame/reg-event-fx
 ::load
 (fn [_ _]
   {:dispatch-n
    (map (fn [url] [:geopub.rdf/get url {:content-type "text/turtle"
                                         :on-success [::db/add-rdf-graph]}])
         ["/activitystreams2.ttl" "/schema.ttl" "/rdf.ttl"])}))
