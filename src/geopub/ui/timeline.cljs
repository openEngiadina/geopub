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

(ns geopub.ui.timeline
  (:require-macros [cljs.core.logic :refer [run* fresh]])
  (:require [geopub.ns :refer [as rdfs schema]]
            [geopub.ui.utils :refer [iri-component literal-component]]
            [cljs.core.logic :as l]
            [cljs-rdf.core :as rdf]
            [cljs-rdf.graph.set]))

(defmulti object-component
  (fn [object] (first (rdf/description-get object (rdf/rdf :type)))))

(defmethod object-component
  (as :Note)
  [object]
  [:div.object
   (for [content (rdf/description-get object (as :content))]
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
     (map (partial rdf/description-move activity)
          (rdf/description-get activity (as :object)))]

     ^{:key (prn-str (rdf/description-subject object))}
     [object-component object])

   [:div.meta
    [iri-component (rdf/description-get activity (as :actor))]
    [:br]
    [iri-component (rdf/description-get activity (rdf/rdf :type))]]])

(defn get-activities [graph]
  "Returns a list of activities (as RDF Descriptions)"
  ;; A query to get all activities
  ;; TODO only store the relevant subgraph in the description
  (map #(rdf/description % graph)
       (run* [s]
             (fresh [activity-type]
                    (rdf/graph-tripleo graph
                                       (rdf/triple activity-type (rdfs "subClassOf") (as "Activity")))
                    (rdf/graph-tripleo graph
                                       (rdf/triple s (rdf/rdf "type") activity-type))))))

(defn view [state]
  [:div#timeline
   [:h1 "Timeline"]
   (for [activity (get-activities (:store @state))]
     ^{:key (prn-str (rdf/description-subject activity))}
     [activity-component activity])])
