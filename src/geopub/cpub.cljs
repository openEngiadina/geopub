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

(ns geopub.cpub
  "Helpers for interacting with CPub server"
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [rdf.core :as rdf]
            [geopub.data.rdf :refer [get-rdf]]
            [goog.Uri :as uri]))

(defn public-timeline-url [server-url]
  (if (uri? server-url)
    (.setPath server-url "public")
    (public-timeline-url (uri/parse server-url))))

(defn get-public-timeline [server-url]
  "Returns a channel holding the content of the public timeline"
  (-> server-url
      (public-timeline-url)
      (get-rdf)))


;; (defn post-rdf [data url auth]
;;   (go
;;     (let [body (<! (n3/encode data))]
;;       (http/post url
;;                  {:with-credentials? false
;;                   :basic-auth auth
;;                   :headers {"Content-type" "text/turtle"}
;;                   :body body}))))
