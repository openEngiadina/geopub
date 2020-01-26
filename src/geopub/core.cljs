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

(ns geopub.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [geopub.ui :as ui]))


;; ====================== Config =============================================

;; (def server-url "https://ap-dev.miaengiadina.ch/")
(def server-url "http://localhost:8080/")



;; =================== State and helpers =====================================

(defonce state (r/atom {}))

(defn set-selected! [state selected]
  "Set selected activity/object"
  (swap! state assoc :selected selected))

(defn is-selected? [state thing]
  "Is the thing selected?"
  (or
   (= (:selected @state) thing)
   (= (:selected @state) (:id thing))))

(defn set-hovered! [state hovered]
  "Set the hovered on activity/object"
  (swap! state assoc :hover hovered))

(defn is-hover? [state thing]
  "Is the thing selected?"
  (or
   (= (:hover @state) thing)
   (= (:hover @state) (:id thing))))

(defn set-position! [state pos]
  (swap! state assoc :position pos))

(defn get-position [state]
  (:position @state))

(defn get-public-activities [state]
  (get-in @state [:public :items]))

(defn get-public-objects [state]
  (distinct
   (map :object (get-in @state [:public :items]))))

(defn get-liked-objects [state]
  (get-in @state [:liked :items]))

(defn is-liked? [state object]
  (some (fn [id] (= (:id object) id))
        (map :id (get-liked-objects state))))

(defn get-actor-outbox [state]
  (get-in @state [:actor :outbox]))

(defn post-activity! [activity]
  (http/post (get-actor-outbox state) {:with-credentials? false :json-params activity}))

(defn like-object! [object]
  (post-activity! {:type "Like"
                   :to "https://www.w3.org/ns/activitystreams#Public"
                   :object (or (:id object) object)}))

;; ===========================================================================


(defn http-get [url]
  (http/get url {:with-credentials? false}))

(defn reset-database! []
  (go
    (<! (http-get (str server-url "api/dev/reset")))))

(defn get-actor! []
  ;; Get the ActivityPub Actor along with inbox/outbox
  (go
    (let
     [actor-profile (:body (<! (http-get (:actor-id @state))))
      liked (:body (<! (http-get (:liked actor-profile))))]

      (swap! state
             #(assoc % :actor actor-profile))

      (swap! state
             #(assoc % :liked liked)))))

(defn get-public! []
  ;; Get the public collection
  (go
    (let
     [public (:body (<! (http-get (str server-url "public"))))]
      (swap! state #(assoc % :public public)))))

(defn refresh! []
  ;; Refresh data from server (Actor and public collection)
  (get-actor!)
  (get-public!))



(r/render [ui/ui]
          (js/document.getElementById "app")
          refresh!)

(defonce refresher
  (js/setInterval #(refresh!) 20000))

