(ns geopub.rdf.http-get
  (:require [rdf.core :as rdf]
            [rdf.parse :as rdf-parse]
            [async-error.core]
            [cljs.core.async :as async :refer [<!]]
            [cljs-http.client :as http]
            [re-frame.core :as re-frame])
  (:require-macros [async-error.core :refer [<? go-try]]
                   [cljs.core.async :refer [go]]))

;; TODO Parser is capable of parsing streams, but cljs-http returns entire body in one go. Explore using the Streams API (https://developer.mozilla.org/en-US/docs/Web/API/Streams_API).

(defn- map-content-types
  "Some projects and services do not use standard content-types for some reason or an other."
  [ct]
  (condp = ct
    ;; https://www.w3.org/TR/activitystreams-core/#media-type
    "application/activity+json" "application/ld+json"
    ct))

(defn- get-content-type [response]
  (-> response
      (get-in [:headers "content-type"])
      (clojure.string/split ";")
      (first)
      (map-content-types)))

(defn- wrap-request
  "Per default cljs-http parses JSON. We do not want this."
  [request]
  (-> cljs-http.core/request
      cljs-http.client/wrap-accept
      cljs-http.client/wrap-form-params
      cljs-http.client/wrap-multipart-params
      cljs-http.client/wrap-edn-params
      cljs-http.client/wrap-edn-response
      cljs-http.client/wrap-transit-params
      cljs-http.client/wrap-transit-response
      cljs-http.client/wrap-json-params
      ;; wrap-json-response
      cljs-http.client/wrap-content-type
      cljs-http.client/wrap-query-params
      cljs-http.client/wrap-basic-auth
      cljs-http.client/wrap-oauth
      cljs-http.client/wrap-method
      cljs-http.client/wrap-url
      cljs-http.client/wrap-channel-from-request-map
      cljs-http.client/wrap-default-headers))

(defn http-get
  "Do a HTTP Get request and attempt to parse respose as RDF. Returns a channel holding an RDF graph or an error."
  [url & [opts]]
  (go-try
    (let [request-opts (merge {:headers
                               {"Accept" (clojure.string/join
                                          ", " (rdf-parse/content-types))}
                               :method :get
                               :url (if (rdf/iri? url) (rdf/iri-value url) url)}
                              opts)
          ;; cljs-http does too much post-processing (such as parsing json)
          request (wrap-request cljs-http.core/request)

          response (<? (request request-opts))]

      (if (:success response )

        ;; Parse RDF triples and add to a new graph
        (<? (async/reduce

             (fn [graph triple]
               (cond
                 ;; handle errors
                 (instance? js/Error triple) triple
                 (instance? js/Error graph) graph
                 :else (rdf/graph-add graph triple)))
             ;; initialize a fresh graph
             (rdf.graph.map/graph)
             ;; receive parsed triples in  channel
             (rdf-parse/parse-string (:body response)
                                     :content-type (or (:content-type opts)
                                                       (get-content-type response))
                                     :base-iri (str url)
                                     :path (str url))))

        ;; HTTP request failed. Return an error.
        (ex-info "HTTP request failed" response)))))


(re-frame/reg-fx
 ::http-get-rdf
 (fn [args]
   (go (let [[url opts] args
             graph (<! (http-get url opts))]
         (if (rdf/graph? graph)
           (when-let [on-success (:on-success opts)]
             (re-frame/dispatch (conj on-success graph)))
           (when-let [on-error (:on-error opts)]
             (re-frame/dispatch (conj on-error graph))))))))

(comment
  (go-try
    (let [schema (<? (http-get "schema.ttl" {:content-type "text/turtle"}))]
      (print schema))))
