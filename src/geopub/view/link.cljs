(ns geopub.view.link
  "component for rendering in app links"
  (:require [re-frame.core :as re-frame]
            [reitit.frontend.controllers :as rfc]
            [reitit.frontend.easy :as rfe]))

(defn link-component [body & route]
  "Link to internal route"
  [:a {:href (apply rfe/href route)
            :on-click (fn [e]
                        ;; prevent browser from loading the href
                        (.preventDefault e)
                        (re-frame/dispatch
                         (vec (cons :geopub.router/navigate route))))}
        body])
