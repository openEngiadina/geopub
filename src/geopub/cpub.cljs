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
  (:require [re-frame.core :as re-frame]

            [rdf.core :as rdf]

            [geopub.db :as db]
            [geopub.ns :refer [ldp as]]
            [geopub.cpub.oauth :as oauth]
            [geopub.rdf.view :refer [description-label-component]]

            [goog.Uri :as uri]))

(re-frame/reg-sub
 ::user-profile
 (fn [_] [(re-frame/subscribe [::oauth/userinfo])
          (re-frame/subscribe [::db/graph])])
 (fn [input _]
   (let [[userinfo graph] input]
     (when (rdf/graph? graph)
       (some-> (:sub userinfo)
               (rdf/iri)
               (rdf/description graph))))))

(comment)
(rdf/graph?
 (rdf/description-graph @(re-frame/subscribe [::user-profile])))

(defn get-profile-component []
  "component that gets the inbox and outbox whenever userinfo changes"
  (let [userinfo (re-frame/subscribe [::oauth/userinfo])]
    (when-let [sub (:sub @userinfo)]
      (re-frame/dispatch [:geopub.rdf/get {:uri sub
                                           :on-success [::db/add-rdf-graph]}]))))

(defn get-inbox-outbox-component []
  (let [user-profile (re-frame/subscribe [::user-profile])]
    (when (rdf/description? @user-profile)
      [description-label-component @user-profile])))

;; (rdf/description-get-first profile (ldp "inbox"))

(comment
  (defn like! [state what]
    (go-try
     (let [like-post (<? (-> (activity/like what)
                             (geopub.data.rdf/post-rdf
                              (get-in @state [:account :outbox])
                              {:basic-auth (get-in @state [:account :basic-auth])
                               :with-credentials? false})))
           like-activity (<?
                          (get-rdf (get-in like-post [:headers "location"])
                                   {:with-credentials? false}))]
       (swap! state
              (geopub.state/merge-graphs like-activity))))))
