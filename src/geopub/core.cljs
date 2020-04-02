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

;; ============== Initialize state ===============

(defonce state (geopub.state/init))

;; ============== Start fetching data ============

;; Some public accessible data that is loaded on init
(def sample-data
  [ ;; Some websites that have RDFa markup
   "https://inqlab.net/"
   "https://openengiadina.net/"
   "https://ruben.verborgh.org/"
   "https://www.rubensworks.net/"

   ;; Get some ActivityPub actors and some activities from their outboxes
   "https://chaos.social/users/pukkamustard"
   "https://chaos.social/users/pukkamustard/outbox?page=true"

   "https://mastodon.xyz/users/NGIZero"
   "https://mastodon.xyz/users/NGIZero/outbox?page=true"

   "https://mastodon.social/users/eff"
   "https://mastodon.social/users/eff/outbox?page=true"

   "https://mastodon.social/users/sl007"
   "https://mastodon.social/users/sl007/outbox?page=true"

   "https://framapiaf.org/users/framasoft"
   "https://framapiaf.org/users/framasoft/outbox?page=true"

   "https://literatur.social/users/buechergefahr"
   "https://literatur.social/users/buechergefahr/outbox?page=true"

   ;; Events, organizations and places are available from radar.squat.net
   "https://radar.squat.net/en"

   ;; local development instance of CPub
   "http://localhost:4000/public"

   ;; CPub instance on openengiadina.net
   "https://openengiadina.net/public"])

(defn load-public-data [srcs]
  (async/merge
   (map #(geopub.state/add-rdf-graph!
          state (get-rdf % {:with-credentials? false}))
        srcs)))

;; Ontologies that are bundled with GeoPub
(def default-ontologies
  ["activitystreams2.ttl" 
   "schema.ttl"
   "rdf.ttl"])

(defn load-ontologies []
  (async/merge
   (map #(geopub.state/add-rdf-graph!
          state (get-rdf % {:content-type "text/turtle"}))
        default-ontologies)))

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
      ;; [:li [:a {:href (rfe/href :geopub.routes/store)} "Store"]]
      ;; [:li [:a {:href (rfe/href :geopub.routes/map)} "Map"]]
      ]

     [:ul.nav-right
      [:li [:a {:href (rfe/href :geopub.routes/settings)} "About"]]
      [:li [:a {:href (rfe/href :geopub.routes/settings)} "Settings"]]
      ]]
    ]

   (if (:loading-initial @state)
     [:div.ui-page
      "Loading some initial data ... "]
     (let [view (get-in @state [:current-route :data :view])]
       (if view
         [view state]
         [default-view state])))])


(defn init! []

  ;; set the loading bit
  (swap! state #(assoc % :loading-initial true))
  
  (async/take!
   (async/reduce (constantly :done) :blups
                 (async/merge [(load-ontologies)
                               (load-public-data sample-data)]))
   (fn []
     (print "Finished loading init/sample data.")
     (swap! state #(dissoc % :loading-initial))))

  (rfe/start!
   (rf/router geopub.routes/routes)
   (fn [match]
     (swap! state #(assoc % :current-route match)))
   ;; set to false to enable HistoryAPI
   {:use-fragment true})
  (r/render [ui state]
            (.getElementById js/document "app")
            #(print "GeoPub mounted.")))
