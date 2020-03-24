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
            [geopub.ui.map]
            [geopub.ui.store]
            [geopub.ui.activity]
            [geopub.ui.browse]
            [geopub.data.rdf :refer [get-rdf]]
            [geopub.cpub :as cpub]
            [geopub.routes]
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

(defn cpub-get-data! []
  "Get data from CPub server"
  ;; get public timeline
  (geopub.state/add-rdf-graph! state
                               (cpub/get-public-timeline server-url))
  ;; get actor profile
  (geopub.state/add-rdf-graph! state
                               (get-rdf actor-id
                                        {:auth auth
                                         :with-credentials? false}))

  ;; get actor inbox TODO: figure out outbox from actor object
  (geopub.state/add-rdf-graph! state (get-rdf (str actor-id "/inbox")
                                              {:basic-auth auth
                                               :with-credentials? false
                                               }))
  ;; get actor outbox
  (geopub.state/add-rdf-graph! state (get-rdf (str actor-id "/outbox")
                                              {:basic-auth auth
                                               :with-credentials? false})))


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
      [:li [:a {:href (rfe/href :geopub.routes/browse)} "Browse"]]
      [:li [:a {:href (rfe/href :geopub.routes/store)} "Store"]]
      [:li [:a {:href (rfe/href :geopub.routes/map)} "Map"]]]]]

    (let [view (get-in @state [:current-route :data :view])]
      (if view
        [view state]
        [default-view state]))])

(defn init! []
  (load-ontologies)
  (rfe/start!
   (rf/router geopub.routes/routes)
   (fn [match]
     (swap! state #(assoc % :current-route match)))
    ;; set to false to enable HistoryAPI
   {:use-fragment true})
  (r/render [ui state]
            (.getElementById js/document "app")
            cpub-get-data!))


;; how to like something:
;; (-> (activitypub/like (rdf/iri "http://openengiadina.net/"))
;;     (cpub/post-rdf (str actor-id "/outbox") auth))

;; (cpub-get-data!)

;; NOTE: The mystery why the size of the store increases when loading the ontology: Blank Nodes. N3.js gives new ids so blank nodes (and thing refering those blank nodes) are duplicted...need metadata
;; (load-ontologies)

;; (geopub.state/reset-graph! state)
;; (geopub.state/add-rdf-graph! state (get-rdf "https://openengiadina.net/"
;;                                           {:with-credentials? false}))

;; (geopub.state/add-triples! state (get-rdf "https://inqlab.net/"
;;                                           {:with-credentials? false}))

;; Load some stuff to play around

(geopub.state/add-triples! state (get-rdf "https://ruben.verborgh.org"
                                          {:with-credentials? false}))

(geopub.state/add-triples! state (get-rdf "https://chaos.social/users/pukkamustard"
                                          {:with-credentials? false}))

(geopub.state/add-triples! state (get-rdf "https://chaos.social/users/bumbleblue"
                                          {:with-credentials? false}))

(geopub.state/add-triples! state (get-rdf "https://mastodon.social/users/sl007"
                                          {:with-credentials? false}))

(geopub.state/add-triples! state (get-rdf "https://framapiaf.org/users/srosset"
                                          {:with-credentials? false}))

(geopub.state/add-triples! state (get-rdf "https://literatur.social/users/buechergefahr"
                                          {:with-credentials? false}))



