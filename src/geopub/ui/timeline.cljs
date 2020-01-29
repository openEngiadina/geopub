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
  (:require [geopub.ns :refer [as rdfs]]
            [cljs.core.logic :as l]
            [cljs-rdf.core :as rdf]
            [cljs-rdf.graph.set]))

(defn object-component [object]
  [:div.object "hi"])

(defn activity-component [activity]
  [:div.activity
   [:div.meta
    [:p
     [:span.actor (prn-str (rdf/description-get activity (as :actor)))]
     [:span.type (prn-str (rdf/description-get activity (rdf/rdf :type)))]]]

   ;; render object
   (for
       [object
        (map (partial rdf/description-move activity)
             (rdf/description-get activity (as :object)))]

     ^{:key (prn-str (rdf/description-subject object))}
     [object-component object])])

(defn get-activities [graph]
  "Returns a list of activities (as RDF Descriptions)"
  ;; A query to get all activities
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
