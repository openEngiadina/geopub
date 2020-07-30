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
            [geopub.rdf]
            [geopub.rdf.ontology :as ontology]
            [clojure.core.logic :as l]))

(re-frame/reg-event-fx
 ::load-init-data
 (fn [_ _]
   {:dispatch-n
    (map (fn [url] [:geopub.rdf/get url {:with-credentials? false
                               :on-success [::db/add-rdf-graph]}])
         ["http://localhost:4000/public"
          ;; "https://openengiadina.net/public"
          ])}))

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
    (reagent/render [view/app] root-el)))

(defn reload! []
  (println "reload!")
  (mount-app))

(defn init! []

  ;; set up dev helpers
  (dev-setup)

  ;; initialize the application database
  (re-frame/dispatch-sync [::db/initialize])

  ;; load initial ontologies
  (re-frame/dispatch [::ontology/load])

  ;; load init data
  (re-frame/dispatch [::load-init-data])

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
  (-> @re-frame.db/app-db
      (:graph)
      (rdf.core/graph-match (rdf.core/triple (l/lvar)
                                             (l/lvar)
                                             (l/lvar)))
      (count))


  (re-frame/dispatch [::ontology/load]))

(comment
  (re-frame/dispatch [::db/initialize])
  (re-frame/dispatch [::db/add-rdf-graph
                      (rdf.core/graph-add
                       (rdf.graph.map/graph)
                       (rdf.core/triple "a" "b" "c"))])
  @re-frame.db/app-db)



