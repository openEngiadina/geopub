(ns geopub.rdf
  "Helpers for getting and posting RDF data"
  (:require [cljs.core.async :as async]
            [re-frame.core :as re-frame]
            [ajax.core :as ajax]
            [ajax.protocols :as pr]
            [day8.re-frame.http-fx]

            [rdf.core :as rdf]
            [rdf.graph.map]
            [rdf.fragment-graph :as fragment-graph]
            [rdf.parse]
            [rdf.skolem]

            [eris.core :as eris]
            [eris.cache]

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

(defn content-addressable-fragment-graph [fg]
  (go (let [eris-urn (<! (eris/iri (fragment-graph/->csexp fg)))]
        (fragment-graph/set-base-subject fg eris-urn))))

(defn parse-content-addressed [data opts]
  "Parse some RDF and make it content-addressed"
  ;; Parse triples into a channel
  (let [out (async/chan)]
    (async/pipeline-async 1 out eris.cache/cache-fragment-graph-async
                          (rdf.parse/parse-string data
                                                  :content-type (:content-type opts)
                                                  :path (:uri opts)
                                                  ;; skolemize and group into fragment graphs
                                                  :xform (comp (fragment-graph/into-fragment-graphs)
                                                               (rdf.skolem/skolemize))))
    out))

(defn parse [data opts]
  "Parse some RDF and return as graph"
  (async/reduce

   (fn [graph triple]
     (if (rdf/triple? triple)
       (rdf/graph-add graph triple) graph))

   ;; initialize a fresh graph
   (rdf.graph.map/graph)
   ;; receive parsed triples in  channel
   (rdf.parse/parse-string data
                           :content-type (:content-type opts)
                           :path (:uri opts)
                           :xform (rdf.skolem/skolemize))))

(comment
  (go
    (println
     (<! (async/into []
                     (parse-content-addressed
                      "<http://example.com/> <http://blups.com> <http://blips.com> .\n<http://bl.com> <http://bl.com#a> <http://123.com> ." {:content-type "text/turtle"}))))))

(defn rdf-response-format [opts]
  "Custom RDF response format for cljs-ajax (see https://github.com/JulianBirch/cljs-ajax/blob/master/docs/formats.md)"
  {:description "RDF"
   :read (fn [xhrio]
           (let [content-type (or (:content-type opts)
                                  (get-content-type xhrio))
                 body (pr/-body xhrio)]
             ;; returns a channel
             (if (:disable-content-addressing opts)
               (parse body {:content-type content-type :uri (:uri opts)})
               (parse-content-addressed body {:content-type content-type :uri (:uri opts)}))))
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
     (go (println (<! (async/into [] response))))
     {}))

  (re-frame/dispatch [::get
                      {:method :get
                       :uri "http://localhost:4000/objects?iri=urn:erisx:AAAABEB6W7PGNETW6HQ36XR5HT736RZNS4JFDLCZN7K42JGIC5HOT4L2WLQHLY2JUOIHJKDPL45NATIIQY2PQJUA7WQUJUN7JQ7ES3EDN6GA"
                       ;; :on-success [::db/add-rdf-graph]
                       :on-success [::get-success]
                       :on-failure [::get-failure-default-handler]}])

  (re-frame/dispatch [::get
                      {:method :get
                       ;; :uri "http://localhost:4000/objects?iri=urn:erisx:AAAABEB6W7PGNETW6HQ36XR5HT736RZNS4JFDLCZN7K42JGIC5HOT4L2WLQHLY2JUOIHJKDPL45NATIIQY2PQJUA7WQUJUN7JQ7ES3EDN6GA"
                       :uri "http://localhost:4000/public"
                       :on-success [::db/add-rdf-graph]
                       ;; :on-success [::get-success]
                       :on-failure [::get-failure-default-handler]}])

  (re-frame/dispatch [::db/initialize])

  (re-frame/dispatch [::get
                      {:method :get
                       :uri "/ontology/eris-cache.ttl"
                       :disable-content-addressing true
                       ;; :on-success [::db/add-rdf-graph]
                       :on-success [::get-success]
                       :on-failure [::get-failure-default-handler]}]))
