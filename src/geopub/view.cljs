(ns geopub.view
  (:require [re-frame.core :as re-frame]
            [reitit.frontend.easy :as rfe]
            [geopub.router :as router]
            [geopub.ns :as ns]))

(defn router-component []
  (let [current-route @(re-frame/subscribe [::router/current-route])]
    (when current-route
      [(-> current-route :data :view)])))

(defn link-component [body & route]
  [:li [:a {:href (apply rfe/href route)
            :on-click (fn [e]
                        ;; prevent browser from loading the href
                        (.preventDefault e)
                        (re-frame/dispatch
                         (vec (cons ::router/navigate route))))}
        body]])

(defn app []
  [:div#container

   [:div#topbar

    [:header
     [:h1 "GeoPub"]]

    [:nav
     [:ul
      [link-component "Activity" ::router/activity]
      [link-component "Browse" ::router/browse (ns/as "Note")]]
     
     [:ul.nav-right
      [link-component "About" ::router/about]
      [link-component "Settings" ::router/settings]]]]
   [router-component]
   ])

