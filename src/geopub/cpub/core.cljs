;; Copyright © 2020 pukkamustard <pukkamustard@posteo.net>
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

(ns geopub.cpub.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<! poll!]]
            [cljs-rdf.core :as rdf]
            [clojure.pprint :refer [pprint]]))


(defn get-objects [server-url auth]
  "Return all objects as a sequence of quads in a channel"
  (go
    (let
        [body
         (:body (<! (http/get server-url
                           {:with-credentials? false
                            :basic-auth {:username "alice" :password "123"}
                            :headers {"Accept" "text/turtle"}
                            })))]
      (rdf/parse-turtle body))))

(def blups
  (get-objects "http://localhost:4000/objects/" {}))

(poll! blups)
