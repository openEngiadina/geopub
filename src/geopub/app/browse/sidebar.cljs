(ns geopub.app.browse.sidebar
  (:require [reagent.core :as r]
            [re-frame.core :as re-frame]
            [reitit.frontend.easy :as rfe]

            [rdf.core :as rdf]
            [rdf.ns :refer [rdfs owl]]

            [goog.string]

            [geopub.ns :refer [as schema prov]]
            [geopub.view.link :refer [link-component]]))

;; Enter URL component

(defn- enter-url-component []
  (let [input (r/atom "")]
    (fn []
      [:div
       [:input {:type "text"
                :on-change #(reset! input (-> % .-target .-value))}]
       [:button
        {:on-click #(rfe/push-state ::browse-description
                                    {:iri (goog.string.urlEncode @input)})}
        "Go"]])))

(defn- url-encode [iri]
  (-> iri
      (rdf/iri-value)
      (goog.string.urlEncode)))

(defn sidebar []
  [:div.sidebar
   [:nav
    [:h4 "ActivityStreams"]
    [:ul
     [:li [link-component "Note" :geopub.app.browse/browse-type
           {:type (url-encode (as "Note"))}]]
     [:li [link-component "Article" :geopub.app.browse/browse-type
           {:type (url-encode (as "Article"))}]]
     [:li [link-component "Person" :geopub.app.browse/browse-type
           {:type (url-encode (as "Person"))}]]
     [:li [link-component "Like" :geopub.app.browse/browse-type
           {:type (url-encode (as "Like"))}]]
     [:li [link-component "Object" :geopub.app.browse/browse-type
           {:type (url-encode (as "Object"))}]]]


    [:h4 "PROV"]
    [:ul
     [:li [link-component "Activity" :geopub.app.browse/browse-type
           {:type (url-encode (prov "Activity"))}]]]

    [:h4 "schema.org"]
    [:ul
     [:li [link-component "Event" :geopub.app.browse/browse-type
           {:type (url-encode (schema "Event"))}]]
     [:li [link-component "Organization" :geopub.app.browse/browse-type
           {:type (url-encode (schema "Organization"))}]]
     [:li [link-component "Place" :geopub.app.browse/browse-type
           {:type (url-encode (schema "Place"))}]]
     [:li [link-component "Thing" :geopub.app.browse/browse-type
           {:type (url-encode (schema "Thing"))}]]]
    
    [:h4 "RDFS / OWL"]
    [:ul
     [:li [link-component "Class" :geopub.app.browse/browse-type
           {:type (url-encode (rdfs "Class"))}]]
     [:li [link-component "Ontology" :geopub.app.browse/browse-type
           {:type (url-encode (owl "Ontology"))}]]]

    [:h3 "Enter URL"]
    [enter-url-component]]])
