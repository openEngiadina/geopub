(ns geopub.data.rdf
  "Helpers for displaying RDF data"
  (:require [rdf.core :as rdf]
            [rdf.description :refer [description-get
                                     description-subject]]
            [rdf.ns :as rdf-ns]
            [rdf.n3 :as n3]
            [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [geopub.ns :as ns :refer [as rdfs schema]]
            [goog.string]
            [reitit.frontend.easy :as rfe])
  (:require-macros [cljs.core.async :refer [go]]))

;; Data fetching
 
(defn get-rdf [url & [opts]]
  (go (let [request-opts (merge {:with-credentials? false
                                 :headers {"Accept" "text/turtle"}} opts)
            request (http/get url request-opts)
            body (:body (<! request))]
        (n3/parse body))))

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

(defmulti description-body-component
  "Takes an rdf description and tries to create a nice view."
  (fn [object] (description-type object)))

(defmethod description-body-component
  :default
  [object]
  [:div.object-body
   [description-turtle-component object]])

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


