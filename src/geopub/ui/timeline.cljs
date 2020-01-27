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

(ns geopub.ui.timeline
  (:require-macros [cljs.core.logic :refer [run* fresh]])
  (:require [cljs.core.logic :as l]))

(run* [q z]
  (fresh [x]
    (l/membero q [1 2 3])
    (l/== q x)
    (l/== x z)))

(run* [q]
  (l/membero q [1 2 3])
  (l/pred q #(odd? %)))


(defn view [state]
  [:div#timeline
   [:h1 "Timeline"]
   (prn-str (:store @state))
   ])
