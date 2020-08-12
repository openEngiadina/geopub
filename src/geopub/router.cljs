(ns geopub.router
  (:require [re-frame.core :as re-frame]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.easy :as rfe]
            [geopub.app.activity]
            [geopub.app.browse]
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

(re-frame/reg-event-fx
 ::navigate-external
 (fn [_ [_ url]]
   {::navigate-external! url}))

(re-frame/reg-fx
 ::navigate-external!
 (fn [url]
   (js/window.location.assign url)))

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


;; (re-frame/dispatch [:test])

(def default-route ::activity)

(def routes
  ["/"
   ["activity"
    {:name ::activity
     :view geopub.app.activity/view}]

   ["browse"
    geopub.app.browse/routes]

   ["map"
    {:name ::map
     :view geopub.app.map/view}]

   ["settings"
    geopub.app.settings/routes]

   ["about"
    {:name ::about
     :view geopub.app.about/view}]])
