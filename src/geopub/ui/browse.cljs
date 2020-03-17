(ns geopub.ui.browse
  (:require [rdf.core :as rdf]
            [rdf.description :as rdf-description]
            [reagent.core :as r]
            [goog.string]
            [geopub.data.rdf :refer [description-header-component
                                     description-component]]))


(defn view [state]
  (let [iri (-> @state
                (get-in [:current-route :path-params :iri])
                (goog.string.urlDecode)
                (rdf/iri))
        
        description (r/track #(rdf-description/description iri (:graph @state)))]

    [:div {:id :browse}
     [description-component @description]]))

