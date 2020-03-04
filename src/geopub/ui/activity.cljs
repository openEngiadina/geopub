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
            [geopub.ui.utils :refer [iri-component literal-component]]
            [activitypub.core :as activitypub]
            [rdf.core :as rdf]
            [rdf.description :as rd]
            [rdf.ns :as ns]
            [rdf.graph.map]))

(defmulti object-component
  (fn [object] (first (rd/description-get object (ns/rdf :type)))))

(defmethod object-component
  (as :Note)
  [object]
  [:div.object
   (for [content (rd/description-get object (as :content))]
     [:p [literal-component content]])])

(defmethod object-component
  (schema "Event")
  [object]
  [:div.object "I'm an event"])

(defmethod object-component
  :default
  [object]
  [:div.object "I'm a object"])

(defn activity-component [activity]
  [:div.activity

   ;; render object
   (for
    [object
     (map (partial rd/description-move activity)
          (rd/description-get activity (as :object)))]

     ^{:key (prn-str (rd/description-subject object))}
     [object-component object])

   [:div.meta
    [iri-component (rd/description-get activity (as :actor))]
    [:br]
    [iri-component (rd/description-get activity (ns/rdf :type))]]])

(defn view [state]
  [:div#timeline
   [:h1 "Activity"]
   (for [activity (activitypub/get-activities (:store @state))]
     ^{:key (prn-str (rd/description-subject activity))}
     [activity-component activity])])
