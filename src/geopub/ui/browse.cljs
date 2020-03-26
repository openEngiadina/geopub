(ns geopub.ui.browse
  (:require [rdf.core :as rdf]
            [reagent.core :as r]
            [goog.string]
            [geopub.state]
            [geopub.ns :as ns]
            [geopub.cpub]
            [geopub.data.rdf :refer [rdf-term-component
                                     description-component]]
            [geopub.data.ontology]
            [geopub.data.schema]
            [cljs.core.logic :as l]
            [rdf.logic :as rdf-logic]
            [reitit.frontend.easy :as rfe])
  (:require-macros [cljs.core.logic :refer [run* fresh]]))


(defn browse-href [type]
  (rfe/href :geopub.routes/browse {} {:type (goog.string.urlEncode
                                             (rdf/iri-value type))}))

(defn go-to-url []
  (let [input (r/atom "")]
    (fn []
      [:div
       [:input {:type "text"
                :on-change #(reset! input (-> % .-target .-value))}]
       [:button
        {:on-click #(rfe/push-state :geopub.routes/description
                                    {:iri (goog.string.urlEncode @input)})}
        "Go"]])))

(defn sidebar []
  [:div.sidebar
   [:nav
    [:h3 "Types"]
    [:ul
     [:li [:a {:href (browse-href (ns/as "Note"))} "Notes"]]
     [:li [:a {:href (browse-href (ns/as "Article"))} "Articles"]]
     [:li [:a {:href (browse-href (ns/as "Person"))} "Person"]]
     [:li [:a {:href (browse-href (ns/as "Like"))} "Likes"]]
     [:li [:a {:href (browse-href (ns/schema "Event"))} "Events"]]
     [:li [:a {:href (browse-href (ns/schema "WebPage"))} "Web Page"]]
     [:li [:a {:href (browse-href (ns/rdfs "Class"))} "Class"]]
     ]

    [:h3 "Enter URL"]
    [go-to-url]
    ]])

(defn get-iri [state]
  (-> @state
      (get-in [:current-route :path-params :iri])
      (goog.string.urlDecode)
      (rdf/iri)))

(defn toolbar [state]
  [:div.toolbar

   [:button
    {:on-click
     #(geopub.cpub/like! (get-iri state))} "Like"]

   [:button
    {:on-click
     #(geopub.state/add-rdf-graph!
       state
       (geopub.data.rdf/get-rdf (rdf/iri-value (get-iri state))
                                {:with-credentials? false}))}
    "Fetch more data"]])

(defn description-view [state]
  (let [iri (get-iri state)
        description (r/track #(rdf/description iri (:graph @state)))]

    [:div.ui-page
     [sidebar]
     [:div.main-container
      [toolbar state]
      [:main
       [description-component @description]]]]))

(defn get-type [state]
  (-> @state
      (get-in [:current-route :query-params :type] "")
      (goog.string.urlDecode)
      (rdf/iri)))

(defn get-descriptions
  "Returns sequence of descriptions that have the specified type."
  [graph type]
  ;; TODO implement RDFs subClass
  (map
   #(rdf/description % graph)
   (run* [subject] (rdf-logic/graph-typeo graph subject type))))

(defn browse-view [state]
  (let [type (get-type state)]
    [:div.ui-page
     [sidebar]
     [:div.main-container
      [:main
       [:h1 [rdf-term-component type]]
       [:table
        [:tbody
         (for [desc (get-descriptions (:graph @state) type)]
           ^{:key (hash desc)}
           [:tr
            [:td [rdf-term-component
                  (rdf/description-subject desc)]]]
           )]]
       ]]]))
