(ns geopub.app.browse
  (:require [reagent.core :as r]
            [re-frame.core :as re-frame]
            [reitit.frontend.easy :as rfe]

            [rdf.core :as rdf]
            [rdf.ns :refer [rdfs owl]]
            [rdf.logic :as rdf-logic]
            [cljs.core.logic :as l]

            [geopub.cpub]
            [geopub.ns :refer [as schema]]
            [geopub.rdf.view :refer [rdf-term-component
                                     description-label-component
                                     description-component]]
            [geopub.db :as db]
            [geopub.data.activity]
            [geopub.app.activity]
            [geopub.view.link :refer [link-component]]

            [cljs.core.async :as async])
  (:require-macros [cljs.core.logic :refer [run* fresh]]))

;; common components

(defn- go-to-url []
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

(defn- sidebar []
  [:div.sidebar
   [:nav
    [:h4 "ActivityStreams"]
    [:ul
     [:li [link-component "Note" ::browse-type
           {:type (url-encode (as "Note"))}]]
     [:li [link-component "Article" ::browse-type
           {:type (url-encode (as "Article"))}]]
     [:li [link-component "Actor" ::browse-type
           {:type (url-encode (as "Actor"))}]]
     [:li [link-component "Like" ::browse-type
           {:type (url-encode (as "Like"))}]]
     [:li [link-component "Object" ::browse-type
           {:type (url-encode (as "Object"))}]]]

    [:h4 "schema.org"]
    [:ul
     [:li [link-component "Event" ::browse-type
           {:type (url-encode (schema "Event"))}]]
     [:li [link-component "Organization" ::browse-type
           {:type (url-encode (schema "Organization"))}]]
     [:li [link-component "Place" ::browse-type
           {:type (url-encode (schema "Place"))}]]
     [:li [link-component "Thing" ::browse-type
           {:type (url-encode (schema "Thing"))}]]]
    
    [:h4 "RDFS / OWL"]
    [:ul
     [:li [link-component "Class" ::browse-type
           {:type (url-encode (rdfs "Class"))}]]
     [:li [link-component "Ontology" ::browse-type
           {:type (url-encode (owl "Ontology"))}]]]

    [:h3 "Enter URL"]
    [go-to-url]
    ]])

;; Description view

(re-frame/reg-sub
 ::current-description
 (fn [_] [(re-frame/subscribe [:geopub.db/graph])
          (re-frame/subscribe [:geopub.router/current-route])])
 (fn [input _]
   (let [[graph current-route] input
         subject (-> current-route
                     (get-in [:path-params :iri])
                     (goog.string.urlDecode)
                     (rdf/iri))]
     (when subject
       (rdf/description subject graph)))))

(defn toolbar [description]
  [:div.toolbar
   [:button
    {:on-click #(re-frame/dispatch
                 [:geopub.rdf/get (-> description
                                      (rdf/description-subject)
                                      (rdf/iri-value))
                  {:with-credentials? false
                   :on-success [::db/add-rdf-graph]}])}
    "Load more data"]])

(defn view-description []
  (let [description (re-frame/subscribe [::current-description])
        loading (r/atom false)]
    ;; Auto load some more data if description is empty
    ;; (if (rdf/description-empty? @description)
    ;;   (load-more-data state subject loading))

    [:div.ui-page
     [sidebar]
     [:div.main-container
      [toolbar @description]
      [:main
       [description-component @description]
       ]]
     ;; [activity-bar (:graph @state) subject]
     ]))

;; Type view

(re-frame/reg-sub
 ::current-type-descriptions
 (fn [_] [(re-frame/subscribe [:geopub.db/graph])
          (re-frame/subscribe [:geopub.router/current-route])])
 (fn [input b]
   (let [[graph current-route] input
         ;; extract the type from the current route
         type (-> current-route
                     (get-in [:path-params :type])
                     (goog.string.urlDecode)
                     (rdf/iri))]
     (when type
       [(rdf/description type graph)
        (map #(rdf/description % graph)
             (run* [subject] (rdf-logic/graph-typeo graph subject type)))]))))

(comment
  (deref
   (re-frame/subscribe [::current-type-descriptions])))

(comment
  (-> @re-frame.db/app-db
      (get-in [:current-route :path-params :type])
      (goog.string.urlDecode)
      (rdf/iri)))


(defn view-type []
  (let [type-description-sub (re-frame/subscribe [::current-type-descriptions])
        [type-description objects] @type-description-sub]
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
          (for [desc objects]
            ^{:key (rdf/iri-value (rdf/description-subject desc))}
            [:tr
             [:td [description-label-component desc]]
             [:td [rdf-term-component (rdf/description-subject desc)]]])]]]
       ]]))

(def routes
  [["/description/:iri"
    {:name ::browse-description
     :view view-description
     :parameters {:path {:iri string?}}}]

   ["/type/:type"
    {:name ::browse-type
     :view view-type
     :parameters {:path {:type string?}}}]])





(defn activity-bar [subject]
  [:div.activity-bar
   [:h3 "Related Activity"]
   ;; [geopub.ui.activity/activity-timeline-component
    ;; (geopub.data.activity/get-related-activities graph subject)]
   ])



