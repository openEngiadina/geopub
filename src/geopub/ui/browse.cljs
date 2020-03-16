(ns geopub.ui.browse
  (:require [rdf.core :as rdf]
            [rdf.description :as rdf-description]
            [rdf.n3 :as n3]
            [goog.string]
            [geopub.data.rdf :refer [description-component]]
            [cljs.core.async :refer [<!]]
            [reagent.core :as r])
  (:require-macros [cljs.core.async :refer [go]]))


(defn view [state]
  (let [iri (-> @state
                (get-in [:current-route :path-params :iri])
                (goog.string.urlDecode)
                (rdf/iri))

        description (rdf-description/description iri (:store @state))

        as-turtle (r/atom "Loading...")]
    (fn []
      ;; encode description as RDF/Turtle
      (go (swap! as-turtle (constantly (<! (n3/encode description)))))

      [:div {:id :browse}
       [:h1 "Browse"]

       [description-component description]
       
       [:code @as-turtle]])))
