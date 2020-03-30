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
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [async-error.core :refer [<? go-try]])
  (:require [cljs.core.async :as async :refer [<!]]
            [async-error.core]
            [rdf.core :as rdf]
            [geopub.state]
            [geopub.data.rdf :refer [get-rdf]]
            [geopub.data.activity :as activity]
            [geopub.ns :refer [ldp as]]
            [goog.Uri :as uri]))

(defn public-timeline-url [server-url]
  (if (uri? server-url)
    (.setPath server-url "public")
    (public-timeline-url (uri/parse server-url))))

(defn get-public-timeline [server-url]
  "Returns a channel holding the content of the public timeline"
  (-> server-url
      (public-timeline-url)
      (get-rdf {:with-credentials? false})))

(defn login! [state id basic-auth]
  (go-try
   (let [req-opts {:basic-auth basic-auth
                   ;; Required to allow a authenticated request with CORS. See: https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials
                   :with-credentials? false}
         id (rdf/iri id)
         profile (rdf/description
                  id (<? (get-rdf id req-opts)))
         inbox (<? (get-rdf
                    (rdf/description-get-first profile (ldp "inbox")) req-opts))
         outbox (<? (get-rdf
                     (rdf/description-get-first profile (as "outbox")) req-opts))]

      (swap! state
             (comp
              #(assoc % :account
                      {:basic-auth basic-auth
                       :profile (rdf/description-subject profile)
                       :outbox (rdf/description-get-first profile (as "outbox"))})
              (geopub.state/merge-graphs (rdf/description-graph profile)
                                         inbox
                                         outbox))))))

(defn like! [state what]
  (print (get-in @state [:account :outbox]))
  (-> (activity/like what)
      (geopub.data.rdf/post-rdf
       (get-in @state [:account :outbox])
       {:basic-auth (get-in @state [:account :basic-auth])})))
