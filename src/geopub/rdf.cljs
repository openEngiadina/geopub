(ns geopub.rdf
  (:require [cljs.core.async :as async :refer [<!]]
            [re-frame.core :as re-frame]
            [rdf.core :as rdf]
            [geopub.db :as db]
            [geopub.rdf.http-get])
  (:require-macros [cljs.core.async :refer [go]]))

;; Event to get remote RDF content

(re-frame/reg-event-fx
 ::get
 (fn [_ [_ url & [opts]]]
   {:geopub.rdf.http-get/http-get-rdf [url opts]}))

(comment
  (re-frame/dispatch [::get "schema.ttl"
                      {:content-type "text/turtle"
                       :on-success [::db/add-rdf-graph]}])
  (do
    (:graph @re-frame.db/app-db)))


