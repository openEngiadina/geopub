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
    (map (fn [url] [:geopub.rdf/get {:uri url
                                     :content-type "text/turtle"
                                     ;; don't content-address the ontologies
                                     :disable-content-addressing true
                                     :on-success [::db/add-rdf-graph]}])
         ["/ontology/activitystreams2.ttl"
          "/ontology/schema.ttl"
          "/ontology/rdf.ttl"
          "/ontology/prov.ttl"
          "/ontology/eris-cache.ttl"])}))
