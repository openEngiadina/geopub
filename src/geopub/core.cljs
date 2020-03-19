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
            [goog.Uri :as uri]
            [geopub.state]
            [geopub.ui.map]
            [geopub.ui.store]
            [geopub.ui.activity]
            [geopub.ui.browse]
            [geopub.data.rdf :refer [get-rdf]]
            [geopub.cpub :as cpub]
            [reitit.core :as rc]
            [reitit.frontend :as rf]
            [reitit.frontend.easy :as rfe]))


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
  (geopub.state/add-triples! state
                             (get-rdf "activitystreams2.ttl"
                                      {:content-type "text/turtle"}))
  (geopub.state/add-triples! state
                             (get-rdf "schema.ttl" {:content-type "text/turtle"})))

(defn cpub-get-data! []
  "Get data from CPub server"
  ;; get public timeline
  (geopub.state/add-triples! state
                             (cpub/get-public-timeline server-url))
  ;; get actor profile
  (geopub.state/add-triples! state
                             (get-rdf actor-id {:auth auth}))
  ;; get actor inbox TODO: figure out outbox from actor object
  (geopub.state/add-triples! state (get-rdf (str actor-id "/inbox") {:basic-auth auth}))
  ;; get actor outbox
  (geopub.state/add-triples! state (get-rdf (str actor-id "/outbox") {:basic-auth auth})))


;; ==================== UI =======================

(def default-view geopub.ui.activity/view)

(def routes
  [["activity"
    {:name ::activity
     :view geopub.ui.activity/view}]


   ["browse/description/:iri"
    {:name ::browse
     :view geopub.ui.browse/description-view
     :parameters {:path {:iri string?}}}]

   ["browse/type/:iri"
    {:name ::browse-type
     :view geopub.ui.browse/type-view
     :parameters {:path {:iri string?}}}]

   ["store"
    {:name ::store
     :view geopub.ui.store/view}]

   ["map"
    {:name ::map
     :view geopub.ui.map/view}]])

(defn ui [state]
  [:div#container

   [:div#topbar

    [:header
     [:h1 "GeoPub"]]

    [:nav
     [:ul
      [:li [:a {:href (rfe/href ::activity)} "Activity"]]
      [:li [:a {:href (rfe/href ::store)} "Store"]]
      [:li [:a {:href (rfe/href ::map)} "Map"]]]]]

    (let [view (get-in @state [:current-route :data :view])]
      (if view
        [view state]
        [default-view state]))])

(defn init! []
  (load-ontologies)
  (rfe/start!
   (rf/router routes)
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
;; (geopub.state/add-triples! state (get-rdf "https://www.rubensworks.net/"))
