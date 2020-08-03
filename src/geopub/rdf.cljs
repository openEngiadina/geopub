(ns geopub.rdf
  "Helpers for getting and posting RDF data"
  (:require [cljs.core.async :as async]
            [re-frame.core :as re-frame]
            [ajax.core :as ajax]
            [ajax.protocols :as pr]
            [day8.re-frame.http-fx]
            [rdf.core :as rdf]
            [rdf.graph.map]
            [rdf.parse]
            [rdf.skolem]
            [geopub.ns :refer [as dc]]
            [geopub.db :as db])
  (:require-macros [cljs.core.async :refer [go]]))

(defn- map-content-types
  "Some projects and services do not use standard content-types for some reason or an other."
  [ct]
  (condp = ct
    ;; https://www.w3.org/TR/activitystreams-core/#media-type
    "application/activity+json" "application/ld+json"
    ct))

(defn- get-content-type [xhrio]
  (-> xhrio
      (pr/-get-response-header "content-type")
      (clojure.string/split ";")
      (first)
      (map-content-types)))

(defn parse [data opts]
  (async/transduce

   (rdf.skolem/skolemize)

   (fn [graph triple]
     (if (rdf/triple? triple)
       (rdf/graph-add graph triple) graph))

   ;; initialize a fresh graph
   (rdf.graph.map/graph)
   ;; receive parsed triples in  channel
   (rdf.parse/parse-string data
                           :content-type (:content-type opts)
                           :path (:uri opts))))

(defn rdf-response-format [opts]
  "Custom RDF response format for cljs-ajax (see https://github.com/JulianBirch/cljs-ajax/blob/master/docs/formats.md)"
  {:description "RDF"
   :read (fn [xhrio]
           (let [content-type (or (:content-type opts)
                                  (get-content-type xhrio))
                 body (pr/-body xhrio)]
             ;; returns a channel
             (parse body {:content-type content-type :uri (:uri opts)})))
   :content-type (clojure.string/join ", " (rdf.parse/content-types))})

(re-frame/reg-event-fx
 ::get-failure-default-handler
 (fn [_ [_ response]]
   (println (str "Failed to load RDF data") response)
   {}))

(re-frame/reg-event-fx
 ::get
 (fn [_ [_ opts]]
   {:http-xhrio (merge {:method :get
                        :response-format (rdf-response-format opts)
                        :on-failure [::get-failure-default-handler]}
                       opts)}))

(comment
  (re-frame/reg-event-fx
   ::get-success
   (fn [_ [_ response]]
     (go
       (print (<! response)))
     {}))

  (re-frame/dispatch [::get
                      {:method :get
                       :uri "http://localhost:4000/public"
                       :on-success [::get-success]
                       :on-failure [::get-failure-default-handler]}]))
