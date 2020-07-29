(ns geopub.router
  (:require [re-frame.core :as re-frame]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.easy :as rfe]
            [geopub.app.activity]
            [geopub.app.browse]
            [geopub.app.store]
            [geopub.app.map]
            [geopub.app.settings]
            [geopub.app.about]))

;; event to navigate to another route
(re-frame/reg-event-fx
 ::navigate
 (fn [db [_ & route]]
   {::navigate! route}))

;; Triggering navigation from events.
(re-frame/reg-fx
 ::navigate!
 (fn [route]
   (apply rfe/push-state route)))

;; swap current-route in db after succesfull navigation
(re-frame/reg-event-db
 ::navigated
 (fn [db [_ new-match]]
   (let [old-match   (:current-route db)
         controllers (rfc/apply-controllers (:controllers old-match) new-match)]
     (assoc db :current-route (assoc new-match :controllers controllers)))))

;; subscription to get the current route
(re-frame/reg-sub
 ::current-route
 (fn [db]
   (:current-route db)))

(def default-route ::activity)

(def routes
  [["/activity"
    {:name ::activity
     :view geopub.app.activity/view}]

   ["browse/iri/:iri"
    {:name ::browse-iri
     :view geopub.app.browse/description-view
     :parameters {:path {:iri string?}}}]

   ["browse/blank-node/:blank-node"
    {:name ::browse-blank-node
     :view geopub.app.browse/description-view
     :parameters {:path {:blank-node string?}}}]

   ["browse"
    {:name ::browse
     :view geopub.app.browse/browse-view
     :parameters {:query {:type string?}}}]

   ["store"
    {:name ::store
     :view geopub.app.store/view}]

   ["map"
    {:name ::map
     :view geopub.app.map/view}]

   ["/settings"
    {:name ::settings
     :view geopub.app.settings/view}]

   ["/about"
    {:name ::about
     :view geopub.app.about/view}]
   ])
