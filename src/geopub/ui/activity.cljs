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

(ns geopub.ui.activity
  (:require [geopub.ns :refer [as rdfs schema]]
            [geopub.state]
            [geopub.data.rdf :refer [iri-component
                                     literal-component
                                     description-component]]
            [geopub.data.activity :as activity]
            [rdf.core :as rdf]
            [rdf.description :as rd]
            [rdf.ns :as ns]
            [rdf.graph.map]))

(defn activity-component [activity]
  [:div.activity
   ;; render object
   (for
    [object
     (map (partial rd/description-move activity)
          (rd/description-get activity (as :object)))]

     ^{:key (prn-str (rd/description-subject object))}
     [description-component object])

   [:div.meta
    [iri-component (rd/description-get activity (as :actor))]
    [:br]
    [iri-component (rd/description-get activity (ns/rdf :type))]]])

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

(defn view [state]
  [:div.ui-page
   [sidebar]
   [:main
    [:h1 "Activity"]
    (for [activity (activity/get-activities (:graph @state))]
      ^{:key (hash activity)}
      [activity-component activity])]])
