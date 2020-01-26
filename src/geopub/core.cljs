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
  (:require-macros [cljs.core.async :refer [go]])
  (:require [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [geopub.ui.map]
            [geopub.ui.timeline]
            [geopub.cpub.core :as cpub]
            [reitit.core :as rc]
            [reitit.frontend :as rf]
            [reitit.frontend.easy :as rfe]))


;; ====================== Config =================

;; (def server-url "https://ap-dev.miaengiadina.ch/")


(def server-url "http://localhost:4000/")

(def auth {:username "alice" :password "123"})

;; ============== State and helpers ==============

(defonce state (r/atom {:store []}))

;; ============== Start fetching data ============

(defn get-objects []
  "Get objects from server and place in store."
  (go
    (let [objects (<! (cpub/get-objects (str server-url "objects") auth))]
      (swap! state #(assoc % :store objects)))))

(defn refresh! []
  ;; Refresh data from server (Actor and public collection)
  (do (get-objects)))

;; (defonce refresher
;;   (js/setInterval #(refresh!) 20000))

;; ==================== UI =======================

(def default-view geopub.ui.map/view)

(def routes
  [["timeline"
    {:name ::timeline
     :view geopub.ui.timeline/view}]
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
      [:li [:a {:href "#map"} "Map"]]]

     [:hr]

     [:ul
      [:li [:a {:href "#settings"} "Settings"]]]]

    [:div#debug
     [:code
      (str @state)]]]

   [:main
    (let [view (:view (:current-route @state))]
      (if view
        [view state]
        [default-view state]))]])

(defn init! []
  (rfe/start!
   (rf/router routes)
   (fn [match]
     (swap! state #(assoc % :current-route (:data match))))
    ;; set to false to enable HistoryAPI
   {:use-fragment true})
  (r/render [ui state]
            (.getElementById js/document "app")
            refresh!))

(init!)
