(ns geopub.data.rdf
  "Helpers for displaying RDF data"
  (:require [rdf.core :as rdf]
            [rdf.description :refer [description-get
                                     description-subject]]
            [rdf.ns :as rdf-ns]
            [rdf.n3 :as n3]
            [rdf.parse :as rdf-parse]
            [rdf.graph.map]
            [reagent.core :as r]
            [cljs.core.async :as async :refer [<!]]
            [cljs-http.client :as http]
            [cljs-http.core]
            [geopub.ns :as ns :refer [as rdfs schema]]
            [goog.string]
            [reitit.frontend.easy :as rfe])
  (:require-macros [cljs.core.async :refer [go]]))

;; Data fetching

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

(defn wrap-request
  "Returns a batteries-included HTTP request function coresponding to the given
   core client. See client/request"
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

(defn get-rdf
  "Do a HTTP Get request and attempt to parse respose as RDF. Returns a channel holding an RDF graph or an error."
  [url & [opts]]
  (go
    (let [request-opts (merge {:headers
                               {"Accept" (clojure.string/join
                                          ", " (rdf-parse/content-types))}
                               :method :get
                               :url url}
                              opts)
          ;; cljs-http does too much post-processing (such as parsing json)
          request (wrap-request cljs-http.core/request)

          response (<! (request request-opts))]

      (if (:success response )

        ;; Parse RDF triples and add to a new graph
        (<! (async/reduce

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


;; Reagent components

(defn iri-component
  "Render an IRI as a link that can be followed in the internal browser."
  [iri & {:keys [class]}]
  (let [class (or class "iri")]
    (cond

      (rdf/iri? iri)
      [:span
       {:class class}
       [:a
        {:href (rfe/href :geopub.core/browse
                         {:iri (goog.string.urlEncode (rdf/iri-value iri))})}
        (rdf/iri-value iri)]]

      (seq? iri)
      (iri-component (first iri))

      :else
      [:span.iri "-"])))

(defn literal-component [literal]
  (rdf/literal-value literal))

(defn blank-node-component [bnode]
  (str "_:" (rdf/blank-node-id bnode)))

(defn rdf-term-component [term]
  (cond
    (rdf/iri? term) [iri-component term]

    (rdf/literal? term) [literal-component term]

    (rdf/blank-node? term) [blank-node-component term]

    (seq? term) [rdf-term-component (first term)]

    :else "-"))

;; Description

(defn- description-type
  "Helper to get type of subject being described. This defines what multimethod is used to render the description."
  [object]
  ;; TODO the described object can have multiple types. Currently we use the first type. Allow a preference to be given.
  (first (description-get object (rdf-ns/rdf :type))))

(defmulti description-header-component
  "Render an appropriate title for a description"
  (fn [object] (description-type object)))

(defmethod description-header-component
  :default
  [object]
  [:header [:h1 (rdf-term-component (description-subject object))]])

(defmethod description-header-component
  (rdfs "Class")
  [object]
  (let [title (or (first (description-get object (rdfs "label")))
                  (description-subject object))
        sub-title (first (description-get object (rdfs "comment")))]
    [:header
     [:h1 [rdf-term-component title]
      [:span.raw-id "(" (rdf-term-component (description-subject object)) ")"]]
     (if sub-title [:p.subtitle [rdf-term-component sub-title]])]
    ))

(defn description-turtle-component [object]
  (let [as-turtle (r/atom "")]
    (fn []
      ;; encode description as RDF/Turtle
      (go (swap! as-turtle (constantly (<! (n3/encode object)))))
       [:code.turtle [:pre @as-turtle]])))

(defn description-property-list-component [object]
  [:dl
   (for
       [triple (rdf/triple-seq object)]
     ^{:key (hash triple)}
     [:div.object-property-list
      [:dt [rdf-term-component (rdf/triple-predicate triple)]]
      [:dd [rdf-term-component (rdf/triple-object triple)]]])])

(defmulti description-body-component
  "Takes an rdf description and tries to create a nice view."
  (fn [object] (description-type object)))

(defmethod description-body-component
  :default
  [object]
  [:div.object-body
   [description-property-list-component object]
   ;; [:details
   ;;  [:summary "Turtle"]
   ;;  [description-turtle-component object]]
   ])

(defn description-component
  [object]
  [:section.object
   [description-header-component object]
   [description-body-component object]])

;; (defmethod description-component
;;   (as :Note)
;;   [object]
;;   [:div.object
;;    (for [content (description-get object (as :content))]
;;      [:p [literal-component content]])])

;; (defmethod description-component
;;   (schema "Event")
;;   [object]
;;   [:div.object "I'm an event"])


