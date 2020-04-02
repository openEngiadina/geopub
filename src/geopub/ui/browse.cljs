(ns geopub.ui.browse
  (:require [rdf.core :as rdf]
            [reagent.core :as r]
            [goog.string]
            [geopub.state]
            [rdf.ns :refer [rdfs owl]]
            [geopub.ns :refer [as schema]]
            [geopub.cpub]
            [geopub.data.rdf :refer [rdf-term-component
                                     description-label-component
                                     description-component]]
            [geopub.data.activity]
            [geopub.ui.activity]
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
        {:on-click #(rfe/push-state :geopub.routes/browse-iri
                                    {:iri (goog.string.urlEncode @input)})}
        "Go"]])))

(defn sidebar []
  [:div.sidebar
   [:nav
    [:h4 "ActivityStreams"]
    [:ul
     [:li [:a {:href (browse-href (as "Note"))} "Notes"]]
     [:li [:a {:href (browse-href (as "Article"))} "Articles"]]
     [:li [:a {:href (browse-href (as "Person"))} "Person"]]
     [:li [:a {:href (browse-href (as "Like"))} "Likes"]]
     [:li [:a {:href (browse-href (as "Object"))} "Object"]]]

    [:h4 "schema.org"]
    [:ul
     [:li [:a {:href (browse-href (schema "Event"))} "Events"]]
     [:li [:a {:href (browse-href (schema "Organization"))} "Organization"]]
     [:li [:a {:href (browse-href (schema "Person"))} "Person"]]
     [:li [:a {:href (browse-href (schema "Place"))} "Place"]]
     [:li [:a {:href (browse-href (schema "WebPage"))} "Web Page"]]]

    ;; [:h4 "RDFS"]
    ;; [:ul
    ;;  [:li [:a {:href (browse-href (rdfs "Class"))} "Class"]]]

    [:h3 "Enter URL"]
    [go-to-url]
    ]])

(defn get-subject-from-route [state]
  (condp = (get-in @state [:current-route :data :name])
    :geopub.routes/browse-iri (-> @state
                                  (get-in [:current-route :path-params :iri])
                                  (goog.string.urlDecode)
                                  (rdf/iri))

    :geopub.routes/browse-blank-node (-> @state
                                         (get-in [:current-route :path-params :blank-node])
                                         (goog.string.urlDecode)
                                         (rdf/blank-node))))

(defn toolbar [state]
  (let [subject (get-subject-from-route state)]
    [:div.toolbar

     
     (if (:account @state)
       [:button
        {:on-click
         #(geopub.cpub/like! state subject)} "Like"])

     [:button
      {:on-click
       #(geopub.state/add-rdf-graph!
         state
         (geopub.data.rdf/get-rdf (rdf/iri-value subject)
                                  {:with-credentials? false}))}
      "Fetch more data"]]))

(defn activity-bar [graph subject]
  [:div.activity-bar
   [:h3 "Related Activity"]
   [geopub.ui.activity/activity-timeline-component
    (geopub.data.activity/get-related-activities graph subject)]])

(defn description-view [state]
  (let [subject (get-subject-from-route state)
        description (r/track #(rdf/description subject (:graph @state)))]
    [:div.ui-page
     [sidebar]
     [:div.main-container
      [toolbar state]
      [:main
       [description-component @description]]]
     [activity-bar (:graph @state) subject]]))

(defn get-type [state]
  (-> @state
      (get-in [:current-route :query-params :type] "")
      (goog.string.urlDecode)
      (rdf/iri)))

(defn get-descriptions
  "Returns sequence of descriptions that have the specified type."
  [graph type]
  ;; TODO implement RDFs subClass
  (map #(rdf/description % graph)
       (run* [subject] (rdf-logic/graph-typeo graph subject type))))

(defn browse-view [state]
  (let [type (get-type state)
        type-description (rdf/description type (:graph @state))]
    [:div.ui-page
     [sidebar]
     [:div.main-container
      [:main
       [:h1 "Browse: " [description-label-component type-description]]
       [:table.browse-list
        [:thead
         [:tr
          [:td "Name"]
          [:td "IRI"]]]
        [:tbody
         (for [desc (get-descriptions (:graph @state) type)]
           ^{:key (hash desc)}
           [:tr
            [:td [description-label-component desc]]
            [:td [rdf-term-component (rdf/description-subject desc)]]])]]]]]))
