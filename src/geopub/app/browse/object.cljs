(ns geopub.app.browse.object
  "View a individual object"
  (:require [reagent.core :as r]
            [re-frame.core :as re-frame]

            [rdf.core :as rdf]
            [rdf.logic]
            [cljs.core.logic :as l]

            [eris.cache :refer [ec]]

            [geopub.ns :refer [prov as ogp]]
            [geopub.view.link :refer [link-component]]
            [geopub.rdf.view :refer [description-component
                                     rdf-term-component
                                     description-label-component
                                     sort-descriptions-by-date]]
            [geopub.db :as db]

            [geopub.app.browse.sidebar :refer [sidebar]])
  (:require-macros [cljs.core.logic :refer [run run* fresh]]))

;; get the current IRI from ther route
(re-frame/reg-sub
 ::current-route-iri
 (fn [_] (re-frame/subscribe [:geopub.router/current-route]))
 (fn [current-route _]
   (-> current-route
       (get-in [:path-params :iri])
       (goog.string.urlDecode)
       (rdf/iri))))

;; current description
(re-frame/reg-sub
 ::current-route-description
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

;; Related activity

(defn- related-activityo [graph related-to activity]
  (l/conda
   ;; there is a triple with any property with activity as subject and related-to as object
   [(fresh [p] (rdf.logic/graph-tripleo graph activity p related-to))]
   [l/s# l/u#]))

(defn- get-related-activities
  [graph related-to]
  (->> (run* [activity]
         (rdf.logic/graph-typeo graph activity (as "Activity"))
         (related-activityo graph related-to activity))
       (map #(rdf/description % graph))
       (sort-descriptions-by-date)))

(re-frame/reg-sub
 ::related-activities
 (fn [_] (re-frame/subscribe [:geopub.db/graph]))
 (fn [graph [_ to]]
   (get-related-activities graph to)))

(defn related-activity-bar [description]
  (let [related-activities (re-frame/subscribe
                             [::related-activities (rdf/description-subject description)])]
    [:div.activity-bar
     [:h3 "Related Activity"]
     [geopub.app.activity/activity-timeline-component @related-activities]]))

;; Cache

;; returns descriptions of all ERIS Cache activities that use the given IRI
(re-frame/reg-sub
 ::cache-activities
 (fn [_] (re-frame/subscribe [:geopub.db/graph]))
 (fn [graph [_ iri]]
   (->> (run* [s]
          (rdf.logic/graph-tripleo graph s (prov "used") iri)
          (rdf.logic/graph-typeo graph s (ec "CreateCache")))
        (map #(rdf/description % graph)))))

;; the cached version to display may be specified with the ec query. This returns the ec query param as subscription.
(re-frame/reg-sub
 ::current-route-cache-activity
 (fn [_] [(re-frame/subscribe [:geopub.router/current-route])
          (re-frame/subscribe [:geopub.db/graph])])
 (fn [[current-route graph] _]
   (some-> current-route
           (get-in [:query-params :ec])
           (goog.string.urlDecode)
           (rdf/iri)
           (rdf/description graph))))

(re-frame/reg-sub
 ::cached-description
 (fn [[_ iri]] [(re-frame/subscribe [::current-route-ec])
                (re-frame/subscribe [::cache-activities iri])
                (re-frame/subscribe [:geopub.db/graph])])
 (fn [[current-route-ec cache-activities graph] _]
   (rdf/description current-route-ec graph)))

(defn available-cached-versions-component []
  (let [current-iri @(re-frame/subscribe [::current-route-iri])
        cache-activities @(re-frame/subscribe [::cache-activities current-iri])
        current-cache-activity @(re-frame/subscribe [::current-route-cache-activity])
        ;; TODO make default-cache-activity a subscrition and clean up this mess
        all-cache-activities (re-frame/subscribe [::cache-activities current-iri])
        default-cache-activity (first @all-cache-activities)]
    (when-not (empty? cache-activities)
      [:span
       "cached version of "
       [rdf-term-component current-iri {:disable-href true}]
       " from "
       [:select
        {:on-change (fn [e]
                      (let [ec (.-value (.-target e))]
                        (re-frame/dispatch
                         [:geopub.router/navigate :geopub.app.browse/object
                          {:iri (goog.string.urlEncode (rdf/iri-value current-iri))}
                          {:ec ec}])))}
        (for [a cache-activities]
          ^{:key (hash a)}
          [:option
           ;; TODO and this mess
           (if (or (= (rdf/description-subject current-cache-activity)
                      (rdf/description-subject a))
                   (= (rdf/description-subject default-cache-activity)
                      (rdf/description-subject a)))
             {:selected true
              :value (goog.string.urlEncode (rdf/iri-value (rdf/description-subject a)))}
             {:value (goog.string.urlEncode (rdf/iri-value (rdf/description-subject a)))})
           (rdf/literal-value (rdf/description-get-first a (prov "endedAtTime")))
           ;; [link-component (rdf/iri-value (rdf/description-subject a))
           ;; :geopub.app.browse/object
           ;; {:iri (goog.string.urlEncode (rdf/iri-value current-iri))}
           ;; {:ec (goog.string.urlEncode
           ;; (rdf/iri-value (rdf/description-get-first a (prov "generated"))))}]
           ])]])))

(defn cached-description-component [activity]
  [description-component
   (rdf/description-move activity
                         (rdf/description-get-first activity (prov "generated")))])

(defn cached-data-component []
  (let [current-iri @(re-frame/subscribe [::current-route-iri])
        description @(re-frame/subscribe [::current-route-description])
        current-route-cache-activity (re-frame/subscribe [::current-route-cache-activity])
        all-cache-activities (re-frame/subscribe [::cache-activities current-iri])
        default-cache-activity (first @all-cache-activities)]
    (if (and (some? @current-route-cache-activity)
             (not (rdf/description-empty? @current-route-cache-activity)))
      [cached-description-component @current-route-cache-activity]
      (if (some? default-cache-activity)
        [cached-description-component default-cache-activity]
        [:div
         [description-component description]]))))

(defn cache-toolbar [description]
  (when (rdf/description-empty? description)
    [:div.toolbar
     [available-cached-versions-component]
     [:button
      {:on-click #(re-frame/dispatch
                   [:geopub.rdf/get
                    {:uri (-> description
                              (rdf/description-subject)
                              (rdf/iri-value))
                     :on-success [::db/add-rdf-graph]}])}
      "Load more data"]]))


(defn social-toolbar [description]
  (let [user-profile @(re-frame/subscribe [:geopub.cpub/user-profile])]
    (when user-profile
      [:div.toolbar
       [:button
        {:on-click
         (fn [e]
           (.preventDefault e)
           (re-frame/dispatch [:geopub.cpub/like (rdf/description-subject description)]))}
        "Like"]])))

;; Description view

(defn view []
  (let [description (re-frame/subscribe [::current-route-description])]
    [:div.ui-page
     [sidebar]
     [:div.main-container
      [social-toolbar @description]
      [cache-toolbar @description]
      [:main
       (if-not (rdf/description-empty? @description)
         [description-component @description]
         [cached-data-component (rdf/description-subject @description)])]]
     [related-activity-bar @description]]))

