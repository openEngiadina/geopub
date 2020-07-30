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
            [geopub.view :as view]
            [geopub.rdf]))

(defn load-ontologies []
  "load some ontologies that are bundled with GeoPub"
  (map
   #(re-frame/dispatch-sync [:geopub.rdf/get % {:content-type "text/turtle"
                                           :on-success [::db/add-rdf-graph]}])
   ["activitystreams2.ttl" "schema.ttl" "rdf.ttl"]))

(defn load-init-data []
  (map
   #(re-frame/dispatch-sync [:geopub.rdf/get % {:with-credentials? false
                                           :on-success [::db/add-rdf-graph]}])
   ["http://localhost:4000/public"]))

;; Setup

(defn dev-setup []
  (when goog.DEBUG
    (println "dev mode")
    (enable-console-print!)
    (re-frame/reg-global-interceptor re-frame/debug)))

(defn ^:dev/after-load mount-app []
  ;; clear subscription cache
  (re-frame/clear-subscription-cache!)

  ;; force reagent update (updates everything that does not depend on data)
  (reagent.dom/force-update-all)

  (let [root-el (.getElementById js/document "app")]
    (reagent.dom/unmount-component-at-node root-el)
    (reagent/render [view/app] root-el
                    (fn []
                      (doall
                       (load-ontologies)
                       (load-init-data))))))


(defn init! []

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



