(ns geopub.app.settings
  (:require [cljs.core.async :as async]
            [reagent.core :as r]
            [re-frame.core :as re-frame]

            [geopub.rdf.view :refer [description-label-component]]
            [geopub.cpub :as cpub]
            [geopub.cpub.oauth :as oauth]
            [geopub.local-storage :as local-storage]

            [goog.Uri :as uri]))

(defn authentication-form-component []
  (let [server-url (r/atom "http://localhost:4000/")]
    (fn []
      [:form.authentication
       [:label {:for :server-url} "URL"]
       [:input {:type "url" :id :server-url
                :default-value @server-url
                :required true
                :on-change (fn [e] (swap! server-url #(-> e .-target .-value)))}]

       [:input {:type "submit" :value "Log in"
                :on-click (fn [e]
                            (.preventDefault e)
                            (re-frame/dispatch [::oauth/request-authorization @server-url]))}]])))

(defn authenticated-component [oauth-state]
  (when-let [user-profile (re-frame/subscribe [::cpub/user-profile])]
    (if @user-profile
      [:p "Authenticated as " [description-label-component @user-profile]
       [:br]
       [:button {:on-click #(re-frame/dispatch [::oauth/reset-state])} "Log out"]]
      [:p "Loading user profile ... "])))

(defn oauth-state-debug-component []
  (let [oauth-state @(re-frame/subscribe [::oauth/state])]
    [:pre (prn-str oauth-state)]))

(defn view []
  [:div.ui-page
   [:main
    [:h1 "Settings"]
    [:h2 "Authentication"]
    (let [oauth-state @(re-frame/subscribe [::oauth/state])]
      (cond (:authorized oauth-state) [authenticated-component oauth-state]
            (:error oauth-state) [:div "Error!"
                                  [oauth-state-debug-component]
                                  [:button {:on-click #(re-frame/dispatch [::oauth/reset-state])}
                                   "Try again..."]]
            :else [:div
                   [:button {:on-click (fn [e] (re-frame/dispatch [::oauth/request-authorization "https://openengiadina.net/"]))} "Authenticate with https://openengiadina.net/"]
                   [:details
                    [:summary "custom CPub instance"]
                    [authentication-form-component]]]))]])

;; OAuth Callback route / view

(defn view-oauth-callback []
  (let [current-route @(re-frame/subscribe [:geopub.router/current-route])
        oauth-state @(re-frame/subscribe [::oauth/state])
        params (get current-route :query-params)]
    (if (= oauth-state :external-authorization-request)
      (re-frame/dispatch [::oauth/handle-callback params {}])
      (re-frame/dispatch [:geopub.router/navigate ::main]))
    [:div.ui-page
     [:main
      [:p "Handling OAuth 2.0 callback ... "]
      [oauth-state-debug-component]]]))

(comment
  (re-frame/dispatch
   [::oauth/handle-callback (get-in @re-frame.db/app-db [:current-route :query-params])]))

(def routes
  [["/"
    {:name ::main
     :view view}]
   ["/oauth-callback"
    {:name ::oauth-callback
     :view view-oauth-callback}]])
