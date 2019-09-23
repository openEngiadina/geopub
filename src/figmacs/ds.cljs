(ns figmacs.ds
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]))

(defn- get-tours []
  (http/get "ds-tours.json" {:with-credentials? false}))

(defn post-tours []
  (go
    (let
     [response (<! (get-tours))
      tours (get-in response [:body :data])
      as-activity (fn [object] {:type "Create"
                     :to ["https://www.w3.org/ns/activitystreams#Public"]
                     :object object})]
      (doseq [tour tours]
        (http/post "http://localhost:8080/actors/alice/outbox"
                   {:with-credentials? false :json-params (as-activity tour)})))))

