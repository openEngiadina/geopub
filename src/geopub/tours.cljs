;; Copyright © 2019 pukkamustard <pukkamustard@posteo.net>
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

(ns geopub.tours
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [reagent.core :as r]
            [clojure.string :as str]
            [cljs-http.client :as http]))

(defn post-sample-data [post-activity! url]
  "Load sample data from discover.swiss and post to ActivityPub public collection"
  (go
    (let
     [response (<! (http/get url {:with-credentials? false}))
      tours (get-in response [:body])
      as-activity (fn [object] {:type "Create"
                                :to ["https://www.w3.org/ns/activitystreams#Public"]
                                :object object})]
      (doseq [tour tours]
        (post-activity! (as-activity tour))))))

(defn tour-line [tour]
  (let
      [line-string (get-in tour [:geoShape :line])
       geometry-coordinates (get-in tour [:geometry :coordinates])]

    (cond

      line-string
      (map (fn [x] [(second x) (first x)])
           (map #(str/split % #",")
                (str/split line-string #" ")))

      geometry-coordinates (map (fn [x] [(second x) (first x)])
                                geometry-coordinates)

      :else nil)))


(defn tour? [object]
  "Is the object a tour"
  (or
   (= "discover.swiss/Tour" (:type object))
   (= "DownhillPiste" (:type object))
   (= "AerialWayThing" (:type object))))

(defn tour-id [tour]
  (get tour (keyword "@id")))

(defn tour-image [tour]
  (when (get-in tour [:image :contentUrl])
    [:img.tour-image {:src (get-in tour [:image :contentUrl])}]))

(defn tour-status [status-objects tour]
  "Compute current status of tour from a list of activities (that may include Status targeting the tour)"
  (let
   [status-updates
    (reverse
     ;; TODO: This is a hack as I don't seem capable of sorting by date
     ;; id is monotonically increasing, so this works...hackey
     (sort-by :id
              (filter (fn [status-object] (and (= (:type status-object) "Status")
                                               (= (:target status-object) (tour-id tour))))
                      status-objects)))]
    (first status-updates)))

(defn status-component [status-object timeline?]
  (if status-object
    [:span {:class ["tour-status" (:status status-object)]}
     (if timeline?
       (str (:target status-object)
            " set to "
            (:status status-object)
            " by "
            (:attributedTo status-object))

       (str (:status status-object)
            " (updated " (.fromNow (js/moment (:date status-object)))
            " by "
            (:attributedTo status-object)
            ")"))]
    "-"))

(defn status-update-component [tour submit-status]
  (let
   [status-update-content (r/atom {:status "open"
                                   :type "Status"
                                   :date (.toJSON (js/moment))
                                   :target (tour-id tour)})

    create-activity (fn [object] {:type "Create"
                                  :to "https://www.w3.org/ns/activitystreams#Public"
                                  :object object})

    submit #(do (submit-status (create-activity @status-update-content)))]

    (fn [submit-status]
      [:div.status-update
       [:h3 "Status update"]
       [:select
        {:selected (:status @status-update-content)
         :on-change (fn [event] (swap!
                                 status-update-content
                                 #(assoc % :status (-> event .-target .-value))))}
        [:option {:value "open"} "open"]
        [:option {:value "warning"} "warning"]
        [:option {:value "closed"} "closed"]]
       [:input {:type "button"
                :value "Update"
                :on-click submit}]])))

(defn tour-component [tour submit-status tour-status]
  (when (tour? tour)
    [:div.tour
     [:h3 (get tour :type)]

     [tour-image tour]

     [:dl

      [:dt "Name"]
      [:dd (or (get tour :name) "-")]

      [:dt "ID"]
      [:dd (tour-id tour)]

      [:dt "Description"]
      [:dd (or (get tour :description) "-")]

      (when tour-status
        [:div
         [:dt "Status"]
         [:dd
          [status-component tour-status]]])]

     (when submit-status
       [status-update-component tour submit-status])]))

(defn tours-component [objects is-selected? set-selected! set-hovered! submit-status tour-status]
  [:div
   (for [tour (filter #(tour? %) objects)]
     [:div
      [tour-component tour submit-status (tour-status tour)]
      [:hr]])])
