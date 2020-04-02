(ns geopub.ui.settings
  (:require [cljs.core.async :as async]
            [reagent.core :as r]
            [geopub.data.rdf :refer [description-turtle-component
                                     iri-component]]
            [geopub.state]
            [geopub.cpub]))

(defn channel?
  [x]
  (satisfies? cljs.core.async.impl.protocols/Channel x))

(defn login-component [state]
  (let [input (r/atom {})]
    (fn []
      [:form.login
       [:label {:for :actor-url} "Actor URL"] [:br]
       [:input {:type "url" :id :actor-url
                :placeholder "https://openengiadina.net/users/guest"
                :required true
                :on-change (fn [e]
                             (swap! input
                                    #(assoc % :account-id (-> e .-target .-value))))}]
       [:br]

       [:label {:for :password} "Password"] [:br]
       [:input {:type "password" :id :password
                :required true
                :on-change (fn [e]
                             (swap! input
                                    #(assoc % :password (-> e .-target .-value))))}]
       [:br]

       [:input {:type "submit" :value "Login"
                :disabled (:loading @input)
                :on-click (fn []
                            (swap! input #(assoc % :loading true))
                            (async/take! (geopub.cpub/login! state
                                                             (:account-id @input)
                                                             (:password @input))
                                         (fn [result]
                                           (swap! input #(-> %
                                                             (assoc :result result)
                                                             (dissoc :loading))))))}]

       (if (instance? js/Error (:result @input))
         [:pre.error (prn-str (:result @input))])])))


(defn view [state]
  [:div.ui-page
   [:main
    [:h1 "Settings"]
    [:h2 "Account"]
    (if (:account @state)
      [:div
       [:p "Logged in as " [iri-component (get-in @state [:account :profile])]]
       [:button
        {:on-click #(geopub.state/logout! state)} "Logout"]]

      [login-component state])]])

