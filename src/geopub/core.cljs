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
  (:require [reagent.core :as r]
            [reagent.dom]
            [goog.Uri :as uri]
            [geopub.state]
            [geopub.ns :as ns]
            [geopub.ui.map]
            [geopub.ui.store]
            [geopub.ui.activity]
            [geopub.ui.browse]
            [geopub.data.rdf :refer [get-rdf]]
            [geopub.cpub :as cpub]
            [geopub.routes]
            [cljs.core.async :as async]
            [reitit.core :as rc]
            [reitit.frontend :as rf]
            [reitit.frontend.easy :as rfe]))

(defn ^:dev/after-load start []
  (reagent.dom/force-update-all))

;; ====================== Config =================

(def server-url
  (uri/parse "http://localhost:4000/"))

;; Currently the actor and auth is hardcoded.
(def actor-id (str server-url "users/alice"))
(def auth {:username "alice" :password "123"})

;; ============== State and helpers ==============

(defonce state (geopub.state/init))

;; ============== Start fetching data ============

(defn load-ontologies []
  (geopub.state/add-rdf-graph! state
                             (get-rdf "activitystreams2.ttl"
                                      {:content-type "text/turtle"}))
  (geopub.state/add-rdf-graph! state
                             (get-rdf "schema.ttl" {:content-type "text/turtle"})))


(defn load-public-data [srcs]
  (async/merge
   (map #(geopub.state/add-rdf-graph!
          state (get-rdf % {:with-credentials? false}))
        srcs)))

(defn cpub-get-data! []
  "Get data from CPub server"
  ;; get public timeline
  (geopub.state/add-rdf-graph! state
                               (cpub/get-public-timeline server-url))
  ;; Login
  (geopub.cpub/login! state
                      "http://localhost:4000/users/alice"
                      {:username "alice" :password "123"}))


;; ==================== UI =======================

(def default-view geopub.ui.activity/view)

(defn ui [state]
  [:div#container

   [:div#topbar

    [:header
     [:h1 "GeoPub"]]

    [:nav
     [:ul
      [:li [:a {:href (rfe/href :geopub.routes/activity)} "Activity"]]
      [:li [:a {:href (geopub.ui.browse/browse-href (ns/as "Note"))} "Browse"]]
      [:li [:a {:href (rfe/href :geopub.routes/store)} "Store"]]
      [:li [:a {:href (rfe/href :geopub.routes/map)} "Map"]]]]]

    (let [view (get-in @state [:current-route :data :view])]
      (if view
        [view state]
        [default-view state]))])

;; Some public accessible data that is loaded on init
(def sample-data
  ["https://inqlab.net/"
   "https://openengiadina.net/"
   "https://ruben.verborgh.org/"
   "https://chaos.social/users/pukkamustard"
   "https://chaos.social/users/pukkamustard/outbox?page=true"
   "https://mastodon.social/users/sl007"
   "https://mastodon.social/users/sl007/outbox?page=true"
   "https://framapiaf.org/users/framasoft"
   "https://framapiaf.org/users/framasoft/outbox?page=true"
   "https://literatur.social/users/buechergefahr"
   "https://literatur.social/users/buechergefahr/outbox?page=true"
   "https://radar.squat.net/en"])

(defn init! []
  (load-ontologies)
  (cpub-get-data!)
  (load-public-data sample-data)
  (rfe/start!
   (rf/router geopub.routes/routes)
   (fn [match]
     (swap! state #(assoc % :current-route match)))
    ;; set to false to enable HistoryAPI
   {:use-fragment true})
  (r/render [ui state]
            (.getElementById js/document "app")
            #(print "UI mounted.")))
