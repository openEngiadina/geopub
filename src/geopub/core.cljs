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
            [geopub.ui.core :as ui]
            [geopub.cpub.core :as cpub]))


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

(get-objects)

(defn refresh! []
  ;; Refresh data from server (Actor and public collection)
  (println "Should refresh...TODO"))

;; (defonce refresher
;;   (js/setInterval #(refresh!) 20000))

;; ==================== UI =======================

(r/render [ui/ui state]
          (js/document.getElementById "app")
          refresh!)

