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
  (:require-macros [cljs.core.async :refer [go]]
                   [cljs.core.logic :refer [run* fresh run]])
  (:require [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [cljs.core.logic :as l]
            [clojure.set :refer [intersection]]
            [geopub.ui.map]
            [geopub.ui.store]
            [geopub.ui.timeline]
            [cpub.core :as cpub]
            [activitypub.core :as activitypub]
            [rdf.core :as rdf]
            [rdf.graph.map]
            [reitit.core :as rc]
            [reitit.frontend :as rf]
            [reitit.frontend.easy :as rfe]))


;; ====================== Config =================

;; (def server-url "https://ap-dev.miaengiadina.ch/")


(def server-url "http://localhost:4000/")

;; Currently the actor and auth is hardcoded.
(def actor-id (str server-url "users/alice"))
(def auth {:username "alice" :password "123"})


;; ============== State and helpers ==============

(defonce state (r/atom {:store (rdf.graph.map/graph)}))

(defn state-store [state]
  "Return the datastore"
  (:store @state))

(defn- add-triples-to-store! [state triples]
  "Helper to add triples to the store"
  (swap! state
         (fn [s]
           (assoc s :store
                  (reduce rdf/graph-add (:store s) triples)))))

(defn- go-add-triples-to-store! [chan]
  "Add triples from a channel to the store"
  (go (add-triples-to-store! state (<! chan))))

(defn reset-store []
  "Helper to reset the store"
  (swap! state #(assoc % :store (rdf.graph.map/graph))))

;; ============== Start fetching data ============

(defn load-ontologies []
  (go-add-triples-to-store! (cpub/get-activitystreams-ontology)))

(defn cpub-get-data! []
  "Get data from CPub server"
  ;; get public timeline
  (go-add-triples-to-store! (cpub/get-public-timeline server-url))
  ;; get actor profile
  (go-add-triples-to-store! (cpub/get-rdf actor-id auth))
  ;; get actor inbox TODO: figure out outbox from actor object
  (go-add-triples-to-store! (cpub/get-rdf (str actor-id "/inbox") auth))
  ;; get actor outbox
  (go-add-triples-to-store! (cpub/get-rdf (str actor-id "/outbox") auth)))

;; (defonce refresher
;;   "aka the cpu killer"
;;   (js/setInterval #(cpub-get-data!) 2000))

;; ==================== UI =======================

(def default-view geopub.ui.timeline/view)

(def routes
  [["timeline"
    {:name ::timeline
     :view geopub.ui.timeline/view}]

   ["store"
    {:name ::store
     :view geopub.ui.store/view}]

   ["map"
    {:name ::map
     :view geopub.ui.map/view}]])

(defn ui [state]
  [:div#container
   [:div#sidebar

    [:header
     [:h1 "GeoPub"]]

    [:nav
     [:ul
      [:li [:a {:href "#timeline"} "Timeline"]]
      [:li [:a {:href "#store"} "Store"]]
      [:li [:a {:href "#map"} "Map"]]]

     ;; [:hr]

     ;; [:ul
     ;;  [:li [:a {:href "#settings"} "Settings"]]]
     ]

    ;; [:div#debug
    ;;  [:code
    ;;   (str @state)]]
    ]

   [:main
    (let [view (:view (:current-route @state))]
      (if view
        [view state]
        [default-view state]))]])

(defn init! []
  (load-ontologies)
  (rfe/start!
   (rf/router routes)
   (fn [match]
     (swap! state #(assoc % :current-route (:data match))))
    ;; set to false to enable HistoryAPI
   {:use-fragment true})
  (r/render [ui state]
            (.getElementById js/document "app")
            cpub-get-data!))



;; how to like something:
;; (-> (activitypub/like (rdf/iri "http://openengiadina.net/"))
;;     (cpub/post-rdf (str actor-id "/outbox") auth))

;; (reset-store)
;; (cpub-get-data!)

;; NOTE: The mystery why the size of the store increases when loading the ontology: Blank Nodes. N3.js gives new ids so blank nodes (and thing refering those blank nodes) are duplicted...need metadata
;; (load-ontologies)
