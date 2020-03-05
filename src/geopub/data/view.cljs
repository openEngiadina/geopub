(ns geopub.data.view
  (:require [rdf.core :as rdf]
            [rdf.ns :as rdf-ns]
            [rdf.description :refer [description-get]]
            [geopub.ns :as ns :refer [as rdfs schema]]
            [geopub.ui.utils :refer [iri-component literal-component]]))

(defmulti view
  "Takes an rdf description and tries to create a nice view."
  (fn [object]
    ;; TODO: currently we take the first type. Better would be to take the first best type. Even better allow view-type preference to be specified.
    (first (description-get object (rdf-ns/rdf :type)))))

(defmethod view
  (as :Note)
  [object]
  [:div.object
   (for [content (description-get object (as :content))]
     [:p [literal-component content]])])

(defmethod view
  (schema "Event")
  [object]
  [:div.object "I'm an event"])

(defmethod view
  :default
  [object]
  [:div.object "I'm a object"])

