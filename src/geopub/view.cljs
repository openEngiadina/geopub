(ns geopub.view
  (:require [re-frame.core :as re-frame]
            [reitit.frontend.easy :as rfe]

            [geopub.cpub :as cpub]
            [geopub.cpub.oauth :as oauth]
            [geopub.router :as router]
            [geopub.ns :as ns]
            [geopub.view.link :refer [link-component]]))

(defn router-component []
  (let [current-route @(re-frame/subscribe [::router/current-route])]
    (when current-route
      [(-> current-route :data :view)])))

(defn app []
  [:div#container

   [:div#topbar

    [:header
     [:h1 "GeoPub"]]

    [:nav
     [:ul
      [:li [link-component "Activity" ::router/activity]]
      [:li [link-component "Browse" :geopub.app.browse/browse-type
            {:type (rdf.core/iri-value (ns/as "Note"))}]]]
     
     [:ul.nav-right
      [:li [link-component "About" ::router/about]]
      [:li [link-component "Settings" :geopub.app.settings/main]]]]]
   [router-component]

   ;; auto load userinfo
   [oauth/get-userinfo-component]
   ;; auto load user profile
   [cpub/get-profile-component]
   [cpub/get-inbox-outbox-component]
   ])

