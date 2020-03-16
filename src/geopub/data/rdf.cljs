(ns geopub.data.rdf
  "Helpers for displaying RDF data"
  (:require [rdf.core :as rdf]
            [rdf.description :refer [description-get]]
            [rdf.ns :as rdf-ns]
            [geopub.ns :as ns :refer [as rdfs schema]]
            [goog.string]
            [reitit.frontend.easy :as rfe]))

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

    :else "-"))


;; Description
 
(defmulti description-component
  "Takes an rdf description and tries to create a nice view."
  (fn [object]
    ;; TODO: currently we take the first type. Better would be to take the first best type. Even better allow view-type preference to be specified.
    (first (description-get object (rdf-ns/rdf :type)))))

(defmethod description-component
  (as :Note)
  [object]
  [:div.object
   (for [content (description-get object (as :content))]
     [:p [literal-component content]])])

(defmethod description-component
  (schema "Event")
  [object]
  [:div.object "I'm an event"])

(defmethod description-component
  :default
  [object]
  [:div.object "I'm a object"])

