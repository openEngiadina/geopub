(ns geopub.ui.browse
  (:require [rdf.core :as rdf]
            [rdf.description :as rdf-description]
            [goog.string]
            [geopub.data.view :as data]))


(defn view [state]
  (let [iri (-> @state
                (get-in [:current-route :path-params :iri])
                (goog.string.urlDecode)
                (rdf/iri))
        description (rdf-description/description iri (:store @state))]
    [:div {:id :browse}
     [:h1 "Browse"]
     [data/view description]]))
