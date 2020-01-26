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
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [geopub.ui.core :as ui]))


;; ====================== Config =============================================

;; (def server-url "https://ap-dev.miaengiadina.ch/")
(def server-url "http://localhost:8080/")



;; =================== State and helpers =====================================

(defonce state (r/atom {}))



;; ===========================================================================

(defn refresh! []
  ;; Refresh data from server (Actor and public collection)
  (println "Refreshing"))

(r/render [ui/ui state]
          (js/document.getElementById "app")
          refresh!)

;; (defonce refresher
;;   (js/setInterval #(refresh!) 20000))

