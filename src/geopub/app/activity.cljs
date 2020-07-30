;; Copyright © 2019, 2020 pukkamustard <pukkamustard@posteo.net>
;;
;; This file is part of GeoPub.
;;
;; GeoPub is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GeoPub is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GeoPub.  If not, see <https://www.gnu.org/licenses/>.

(ns geopub.app.activity
  (:require [re-frame.core :as re-frame]
            [geopub.ns :refer [as dc schema]]
            [geopub.db :as db]
            [geopub.rdf.view :refer [iri-component
                                     description-component
                                     description-icon-src
                                     description-label-component
                                     description-created-at]]
            [geopub.data.activity :as activity]
            [rdf.core :as rdf]
            [rdf.ns :refer [rdf rdfs]]
            [rdf.graph.map]
            [rdf.logic :as rdf-logic]
            [cljs.core.logic :as l]
            ["date-fns" :as date-fns])
  (:require-macros [cljs.core.logic :refer [run* run fresh]]))


(defn- sort-by-date [descriptions]
  (sort-by
   ;; get the as:published property and cast to js/Date
   (fn [description]
     (when description
       (description-created-at description)))
   ;; reverse the sort order
   (comp - compare)
   descriptions))

(defn- related-activityo [graph related-to activity]
  (l/conda
   ;; there is a triple with any property with activity as subject and related-to as object
   [(fresh [p] (rdf-logic/graph-tripleo graph activity p related-to))]
   [l/s# l/u#]))

(defn get-related-activities
  [graph related-to]
  (->> (run* [activity]
         (rdf-logic/graph-typeo graph activity (as "Activity"))
         (related-activityo graph related-to activity))
       (map #(rdf/description % graph))
       (sort-by-date)))

(re-frame/reg-sub
 ::activities
 (fn [_] (re-frame/subscribe [::db/graph]))
 (fn [graph _]
   (->> (run* [id] (rdf-logic/graph-typeo graph id (as "Activity")))
        (map #(rdf/description % graph))
        (sort-by-date))))

(comment
  (let [a (re-frame/subscribe [::activities])]
    (count @a)))

(defn published-component [activity]
  (if-some
      [published (description-created-at activity)]
      [:span (.formatDistance date-fns published (new js/Date)
                              (clj->js {:addSuffix true}))]))

(defn activity-component [activity]
  (let
      [object (rdf/description-move
               activity
               (first (rdf/description-get activity (as "object"))))
       actor (rdf/description-move
              activity
              (first (rdf/description-get activity (as "actor"))))

       activity-type (rdf/description-move
                      activity
                      (first (rdf/description-get activity (rdf :type))))]
      [:div.activity
       (if-let [icon-src (description-icon-src actor)]
         [:span.icon [:img {:src (rdf/iri-value icon-src)}]])

       [:span.actor [description-label-component actor]]
       [:span.activity-type [description-label-component activity]]
       [:span.object [description-label-component object]]
       [published-component activity]]))


(defn activity-timeline-component [activities]
  [:div.timeline
   (for [activity activities]
     ^{:key (hash activity)}
     [activity-component activity])])

(defn sidebar []
  [:div.sidebar
   [:nav
    [:p "TODO here you can filter activities"]
    [:ul
     [:li [:a {:href "#"} "you"]]
     [:li [:a {:href "#"} "people you follow"]]
     [:li [:a {:href "#"} "group xyz"]]
     [:li [:a {:href "#"} "all"]]
     ]]])

(defn view []
  (let [activities (re-frame/subscribe [::activities])]
    [:div.ui-page
     ;; [sidebar]
     [:main
      [:h1 "Activity"]
      ;; [:span (count @activities)]
      [activity-timeline-component @activities]
      ]]))
