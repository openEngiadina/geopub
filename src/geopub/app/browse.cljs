(ns geopub.app.browse
  (:require [reagent.core :as r]
            [re-frame.core :as re-frame]

            [rdf.core :as rdf]
            [rdf.ns :refer [rdfs owl]]
            [rdf.logic :as rdf-logic]
            [cljs.core.logic :as l]

            [eris.cache :refer [ec]]

            [geopub.ns :refer [as schema prov]]
            [geopub.rdf.view :refer [rdf-term-component
                                     description-label-component
                                     description-component
                                     sort-descriptions-by-date]]
            [geopub.db :as db]
            [geopub.data.activity]
            [geopub.app.activity]
            [geopub.view.link :refer [link-component]]

            [geopub.app.browse.sidebar :refer [sidebar]]
            [geopub.app.browse.type]

            [cljs.core.async :as async])
  (:require-macros [cljs.core.logic :refer [run run* fresh]]))



;; Current IRI

(re-frame/reg-sub
 ::current-route-iri
 (fn [_] (re-frame/subscribe [:geopub.router/current-route]))
 (fn [current-route _]
   (-> current-route
       (get-in [:path-params :iri])
       (goog.string.urlDecode)
       (rdf/iri))))

;; Cache component
 
(re-frame/reg-sub
 ::cache-activities
 (fn [_] [(re-frame/subscribe [:geopub.db/graph])
          (re-frame/subscribe [::current-route-iri])])
 (fn [input _]
   (let [[graph current-iri] input]
     (->> (run* [s]
            (rdf-logic/graph-tripleo graph s (prov "used") current-iri)
            (rdf-logic/graph-typeo graph s (ec "CreateCache")))
          (map #(rdf/description % graph))
          (group-by #(rdf/description-get-first % (prov "generated")))))))

(defn cached-versions-component [description]
  (let [current-iri (re-frame/subscribe [::current-route-iri])
        cache-activities (re-frame/subscribe [::cache-activities])]
    (when-not (empty? @cache-activities)
      [:div
       [:h4 "Versions"]
       [:ul
        (for [[generated activities] @cache-activities]
          ^{:key (hash generated)}
          (let [generated-desc (rdf/description-move (first activities) generated)]
            [:li [description-label-component generated-desc]]))]])))

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
                 [:geopub.rdf/get
                  {:uri (-> description
                            (rdf/description-subject)
                            (rdf/iri-value))
                   :on-success [::db/add-rdf-graph]}])}
    "Load more data"]])

(defn- related-activityo [graph related-to activity]
  (l/conda
   ;; there is a triple with any property with activity as subject and related-to as object
   [(fresh [p] (rdf-logic/graph-tripleo graph activity p related-to))]
   [l/s# l/u#]))

(defn- get-related-activities
  [graph related-to]
  (->> (run* [activity]
         (rdf-logic/graph-typeo graph activity (as "Activity"))
         (related-activityo graph related-to activity))
       (map #(rdf/description % graph))
       (sort-descriptions-by-date)))

(re-frame/reg-sub
 ::related-activities
 (fn [_] (re-frame/subscribe [:geopub.db/graph]))
 (fn [graph [_ to]]
   (get-related-activities graph to)))

(defn activity-bar [description]
  (let [related-activities (re-frame/subscribe
                             [::related-activities (rdf/description-subject description)])]
    [:div.activity-bar
     [:h3 "Related Activity"]
     [geopub.app.activity/activity-timeline-component @related-activities]]))

(defn view-description []
  (let [description (re-frame/subscribe [::current-description])]
    [:div.ui-page
     [sidebar]
     [:div.main-container
      [toolbar @description]
      [:main
       [description-component @description]
       ;; [cached-versions-component [description]]
       ]]
     [activity-bar @description]]))

(def routes
  [["/description/:iri"
    {:name ::browse-description
     :view view-description
     :parameters {:path {:iri string?}}}]

   ["/type/:type"
    {:name ::browse-type
     :view geopub.app.browse.type/view
     :parameters {:path {:type string?}}}]])

