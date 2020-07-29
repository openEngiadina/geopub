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
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [reitit.frontend :as rf]
            [reitit.frontend.easy :as rfe]
            [geopub.ns :as ns]
            [geopub.db :as db]
            [geopub.router :as router]
            [geopub.view :as view]))

;; (defn ^:dev/after-load start []
;;   (reagent.dom/force-update-all))

;; ============== Initialize state ===============

;; (defonce state (geopub.state/init))

;; ============== Start fetching data ============

;; Some public accessible data that is loaded on init
(comment
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
     "https://openengiadina.net/public"
     "https://openengiadina.net/users/pukkamustard"
     "https://openengiadina.net/users/openengiadina"
     ]))

;; (defn load-public-data [srcs]
;;   (async/merge
;;    (map #(geopub.state/add-rdf-graph!
;;           state (get-rdf % {:with-credentials? false}))
;;         srcs)))

;; Ontologies that are bundled with GeoPub
;; (def default-ontologies
;;   ["activitystreams2.ttl" 
;;    "schema.ttl"
;;    "rdf.ttl"])

;; (defn load-ontologies []
;;   (async/merge
;;    (map #(geopub.state/add-rdf-graph!
;;           state (get-rdf % {:content-type "text/turtle"}))
;;         default-ontologies)))

;; ==================== UI =======================



(defn dev-setup []
  (when goog.DEBUG
    (println "dev mode")
    (enable-console-print!)
    (re-frame/reg-global-interceptor re-frame/debug)))

(defn ^:dev/after-load mount-app []
  ;; clear subscription cache
  (re-frame/clear-subscription-cache!)
  (reagent/render [view/app]
                  (.getElementById js/document "app")))

(defn init! []

  ;; set the loading bit
  ;; (swap! state #(assoc % :loading-initial true))
  
  ;; (async/take!
  ;;  (async/reduce (constantly :done) :blups
  ;;                (async/merge [(load-ontologies)
  ;;                              (load-public-data sample-data)]))
  ;;  (fn []
  ;;    (print "Finished loading init/sample data.")
  ;;    (swap! state #(dissoc % :loading-initial))))
  ;;
  
  ;; set up dev helpers
  (dev-setup)

  ;; initialize the application database
  (re-frame/dispatch-sync [::db/initialize])

  ;; start the reitit router
  (rfe/start!
   (rf/router router/routes)
   (fn [match]
     (if match
       (re-frame/dispatch [::router/navigated match])
       (re-frame/dispatch [::router/navigate router/default-route])))
   ;; set to false to enable HistoryAPI
   {:use-fragment false})

  ;; mount the app
  (mount-app))


(comment
  (re-frame/dispatch [::db/initialize])
  (re-frame/dispatch [::db/add-rdf-graph
                      (rdf.core/graph-add
                       (rdf.graph.map/graph)
                       (rdf.core/triple "a" "b" "c"))])
  @re-frame.db/app-db)



