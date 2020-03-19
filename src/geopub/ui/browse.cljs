(ns geopub.ui.browse
  (:require [rdf.core :as rdf]
            [rdf.description :as rdf-description]
            [reagent.core :as r]
            [goog.string]
            [geopub.data.rdf :refer [description-header-component
                                     description-component]]))

(defn sidebar []
  [:div.sidebar
   [:nav
    [:h3 "Types"]
    [:p "TODO Here you can select type of data to browse."]
    [:ul
     [:li [:a {:href "#"} "Notes"]]
     [:li [:a {:href "#"} "Events"]]
     [:li [:a {:href "#"} "Websites"]]
     [:li [:a {:href "#"} "Actors"]]
     [:li [:a {:href "#"} "Ontologies"]]]]])

(defn get-iri [state]
  (-> @state
      (get-in [:current-route :path-params :iri])
      (goog.string.urlDecode)
      (rdf/iri)))

(defn description-view [state]
  (let [iri (get-iri state)
        description (r/track #(rdf-description/description iri (:graph @state)))]

    [:div.ui-page
     [sidebar]
     [:main [description-component @description]]]))


(defn type-view [state]
  [:div.ui-page
   [sidebar]
   [:main "ylo"]])
