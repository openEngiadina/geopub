(ns geopub.data.rdf
  "Helpers for displaying RDF data"
  (:require [rdf.core :as rdf]
            [rdf.logic :as rdf-logic]
            [rdf.ns :as rdf-ns]
            [rdf.n3 :as n3]
            [rdf.parse :as rdf-parse]
            [rdf.graph.map]
            [reagent.core :as r]
            [cljs.core.logic :as l]
            [cljs.core.async :as async :refer [<!]]
            [cljs-http.client :as http]
            [cljs-http.core]
            [rdf.ns :refer [rdf rdfs owl]]
            [geopub.ns :as ns :refer [as ogp schema foaf dc]]
            [goog.string]
            ["date-fns" :as date-fns]
            [reitit.frontend.easy :as rfe])
  (:require-macros [cljs.core.async :refer [go]]
                   [cljs.core.logic :refer [run fresh]]))

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

(defn get-rdf
  "Do a HTTP Get request and attempt to parse respose as RDF. Returns a channel holding an RDF graph or an error."
  [url & [opts]]
  (go
    (let [request-opts (merge {:headers
                               {"Accept" (clojure.string/join
                                          ", " (rdf-parse/content-types))}
                               :method :get
                               :url (if (rdf/iri? url) (rdf/iri-value url) url)}
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

;; Data posting
 
(defn post-rdf [data url & [opts]]
  (go
    (let [body (<! (n3/encode data))]
      (http/post (if (rdf/iri? url) (rdf/iri-value url) url)
                 (merge
                  {:headers {"Content-type" "text/turtle"}
                   :body body} opts)))))

;; Links to the internal browser
 
(defn- iri-href [iri]
  (rfe/href :geopub.routes/browse-iri
            {:iri (goog.string.urlEncode (rdf/iri-value iri))}))

(defn- blank-node-href [blank-node]
  (rfe/href :geopub.routes/browse-blank-node
            {:blank-node (goog.string.urlEncode (rdf/blank-node-id blank-node))}))

(defn- term-href [term]
  (cond
    (rdf/iri? term) (iri-href term)
    (rdf/blank-node? term) (blank-node-href term)
    :else ""))

;; Reagent components

(defn iri-component
  "Render an IRI as a link that can be followed in the internal browser."
  [iri & [opts]]
  (cond
    (rdf/iri? iri)
    [:span.iri
     (if-not (:disable-href opts)

       ;; create a href
       [:a
        (if-not (:external-href opts)
          ;; to internal browser
          {:href (iri-href iri)}
          ;; or the real (external iri)
          {:href (rdf/iri-value iri) :target "_blank"})
        (rdf/iri-value iri)]

       ;; only display the iri
       (rdf/iri-value iri))]

    (seq? iri)
    (iri-component (first iri))

    :else
    [:span.iri "-"]))

(defn literal-component [literal & [opts]]
  [:span {:dangerouslySetInnerHTML {:__html (rdf/literal-value literal)}}])

(defn blank-node-component [bnode & [opts]]
  (if-not (:disable-href opts)
    [:a {:href (blank-node-href bnode)}
     (str "_:" (rdf/blank-node-id bnode))]
    (str "_:" (rdf/blank-node-id bnode))))

(defn rdf-term-component [term & [opts]]
  (cond
    (rdf/iri? term) [iri-component term opts]

    (rdf/literal? term) [literal-component term opts]

    (rdf/blank-node? term) [blank-node-component term opts]

    (seq? term) [rdf-term-component (first term) opts]

    :else "-"))

;; Description

(defn rdfs-type-labelo [graph subject label]
  (fresh [rdf-type]
    (rdf-logic/graph-tripleo graph (rdf/triple subject (rdf "type") rdf-type))
    (rdf-logic/graph-tripleo graph (rdf/triple rdf-type (rdfs "label") label))))

(defn description-label-term
  "Returns a suitable RDF Term that can be used as a label for the description."
  [object & [opts]]
  (first
   (run 1 [label]
     (l/conda

      ;; use Activitystreams preferredUsername
      ((rdf-logic/description-tripleo object (as "preferredUsername") label))

      ;; use foaf nick
      ((rdf-logic/description-tripleo object (foaf "nick") label))

      ;; use Activitystreams name
      [(rdf-logic/description-tripleo object (as "name") label)]
      
      ;; use dc:title
      [(rdf-logic/description-tripleo object (dc "title") label)]

      ;; use schema.org name
      ((rdf-logic/description-tripleo object (schema "name") label))

      ;; use ogp title
      ((rdf-logic/description-tripleo object (ogp "title") label))

      ;; use rdfs label
      ((rdf-logic/description-tripleo object (rdfs "label") label))

      ;; use the rdfs label of the type
      [(rdfs-type-labelo (rdf/description-graph object)
                         (rdf/description-subject object)
                         label)]

      ;; Fall back to using the subject IRI as label
      [(l/== (rdf/description-subject object) label)]))))


(defn activity-streams-icono [object image]
  (fresh [image-link]
    (rdf-logic/description-tripleo object (as "icon") image-link)
    (rdf-logic/graph-tripleo (rdf/description-graph object)
                             (rdf/triple image-link (as "url") image))))

(defn description-icon-src
  "Returns a string that can be used as src for an icon."
  [object & [opts]]
  (->> (run 1 [image]
         (l/conda
          ((rdf-logic/description-tripleo object (ogp "image") image))

          ((rdf-logic/description-tripleo object (schema "image") image))

          ((activity-streams-icono object image))

          ((l/== image nil))))
       (map (fn [image-term]
              (if (rdf/literal? image-term)
                ;; for some reason ogp stores image as literal..wtf?
                (rdf/iri (rdf/literal-value image-term))
                image-term)))
       (first)))

(defn description-label-component [object & [opts]]
  (let
      [subject (rdf/description-subject object)
       label-term (description-label-term object opts)]

    (if
        (and
         ;; label term is not an iri
         (not (rdf/iri? label-term))
         ;; subject is an iri or blank-node
         (or (rdf/iri? subject)
             (rdf/blank-node? subject))
         ;; we are not disabling hrefs in general
         (not (:disable-href opts)))

      ;; then make the component a clickable link
      [:a {:href (term-href (rdf/description-subject object))}
       [rdf-term-component label-term opts]]
      
      ;; else just display as rdf-term
      [rdf-term-component label-term])))

(defn description-turtle-component [object]
  (let [as-turtle (r/atom "")]
    (fn []
      ;; encode description as RDF/Turtle
      (go (swap! as-turtle (constantly (<! (n3/encode object)))))
      [:code.turtle [:pre @as-turtle]])))

(defn description-property-list-component [desc & [opts]]
  [:dl
   (for
       [triple (cond->> (rdf/triple-seq desc)
                 (contains? opts :properties)
                 (filter #(contains? (:properties opts)
                                     (rdf/triple-predicate %))))]
     
       ^{:key (hash triple)}

       [:div.object-property-list

        [:dt [description-label-component
              (rdf/description-move desc (rdf/triple-predicate triple))]]

        [:dd
         (let [object (rdf/triple-object triple)]
           (if (or (rdf/iri? object) (rdf/blank-node? object))
             [description-label-component (rdf/description-move desc object)]
             [rdf-term-component object]))]])])


(defn description-comment-term
  "Returns a short summary or comment of the description"
  [object]
  (first
   (run 1 [content]
     (l/conda
      [(rdf-logic/description-tripleo object (as "summary") content)]
      [(rdf-logic/description-tripleo object (ogp "description") content)]
      [(rdf-logic/description-tripleo object (rdfs "comment") content)]
      [(l/== content nil)]))))

(defn description-content-term
  "Returns content of the description"
  [object]
  (first
   (run 1 [content]
     (l/conda
      [(rdf-logic/description-tripleo object (as "content") content)]
      [(l/== content nil)]))))

(defn description-created-at
  "Return a date that describes when the object was created"
  [object]
  (some->> (run 1 [c]
             (l/conda
              [(rdf-logic/description-tripleo object (as "published") c)]
              [(l/== c nil)]))
           (first)
           (rdf/literal-value)
           (new js/Date)))

(defn description-creator
  "Returns a description of who created the description"
  [object]
  (some->> (run 1 [c]
             (l/conda
              [(rdf-logic/description-tripleo object (as "actor") c)]
              [(rdf-logic/description-tripleo object (as "attributedTo") c)]
              [(l/== c nil)]))
           (first)
           (rdf/description-move object)))

(def default-metadata
  #{(as "actor")
    (as "to")
    (as "cc")
    (as "inReplyTo")
    (as "attributedTo")
    (as "published")
    (as "outbox")
    (rdf "type")
    (schema "url")})

(defn description-component
  [desc & [opts]]
  [:section.object

   (let [label-term (description-label-term desc)]

     [:header

      ;; Icon
      (if-let [icon-src (description-icon-src desc)]
        [:img.icon {:src (rdf/iri-value icon-src)}])
    
      [:div
       [:h1 [rdf-term-component label-term]]

       ;; display the iri
       [:span.raw-id [rdf-term-component
                      (rdf/description-subject desc)
                      (merge opts {:external-href true})]]

       ;; if there is a comment (or short description) display it
       (if-let [comment (description-comment-term desc)]
         [:p.comment
          [rdf-term-component comment]])

       ;; display common meta data
       [:div.meta
        [description-property-list-component desc
         (merge {:properties default-metadata} opts)]]]])


   (if-let [content (description-content-term desc)]
     [:div.object-content
      [rdf-term-component content]])

   [:details
    [:summary "All Properties"]
    [description-property-list-component desc]]])
