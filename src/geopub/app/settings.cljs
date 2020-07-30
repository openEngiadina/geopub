(ns geopub.app.settings
  (:require [cljs.core.async :as async]
            [reagent.core :as r]
            [re-frame.core :as re-frame]

            [geopub.rdf.view :refer [iri-component]]
            [geopub.cpub.oauth :as oauth]
            [geopub.local-storage :as local-storage]

            [goog.Uri :as uri]))

(defn authentication-form-component []
  (let [server-url (r/atom "http://localhost:4000/")]
    (fn []
      [:form.authentication
       [:label {:for :server-url} "Server URL"]
       [:input {:type "url" :id :server-url
                :default-value @server-url
                :required true
                :on-change (fn [e] (swap! server-url #(-> e .-target .-value)))}]

       [:input {:type "submit" :value "Authenticate"
                :on-click (fn [e]
                            (.preventDefault e)
                            (re-frame/dispatch [::authenticate @server-url]))}]])))

(re-frame/reg-event-fx
 ::authenticate
 [(local-storage/persist :oauth-clients)
  (local-storage/persist :auth-state)]
 (fn [coeffects [_ server-url]]
   (print "oauth-clients: ")
   (println (get-in coeffects [:db :oauth-clients]))

   (print "server-url: ")
   (print server-url)

   (let [client (get-in coeffects [:db :oauth-clients server-url])]
     (cond
       ;; attempt to register a client and retry
       (nil? client) {:dispatch [::oauth/register-client server-url
                                 ;; after attempted client registration authenticate again
                                 {:callback [::authenticate server-url]}]
                      :db (assoc (:db coeffects) :auth-state
                                 {:state :registering-client
                                  :server-url server-url})}

       (= :error client) {:db (assoc (:db coeffects) :auth-state
                                     {:state :error
                                      :server-url server-url})}

       :else {:db (assoc (:db coeffects) :auth-state
                         {:state :external-auth
                          :server-url server-url})
              :dispatch [:geopub.router/navigate-external (authorize-url server-url client)]}))))


(defn authorize-url [server-url client]
  (-> (uri/parse server-url)
      (.setPath "/oauth/authorize")
      (.setQuery (str "response_type=code&"
                      "client_id=" (:client_id client)))
      (.toString)))

(defn view []
  [:div.ui-page
   [:main
    [:h1 "Settings"]
    [:h2 "Account"]
    [authentication-form-component]
    ]])

(defn view-oauth-callback []
  [:div.ui-page
   [:main
    [:p "blups"]]])

(def routes
  [["/"
    {:name ::main
     :view view}]
   ["/oauth-callback"
    {:name ::oauth-callback
     :view view-oauth-callback}]])
