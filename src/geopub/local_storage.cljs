(ns geopub.local-storage
  "re-frame interceptor for persisting db keys in Web API local storage"
  (:require [re-frame.core :as re-frame :refer [->interceptor]]))

(defn persist [key]
  (->interceptor
   :id (keyword (str "local-storage-persist-" key))

   ;; load the key from local storage
   :before (fn [context]
             (let [value (-> (str "geopub-" key)
                             (js/window.localStorage.getItem)
                             (cljs.reader/read-string))]
               (if value
                 (assoc-in context [:coeffects :db key] value)
                 context)))

   :after (fn [context]
            (when-let [value (get-in context [:effects :db key])]
              (js/window.localStorage.setItem (str "geopub-" key)
                                              (prn-str value)))
            context)))


(comment

  (re-frame/reg-event-db
   ::test-event
   (persist :blups)
   (fn [db [_ value]]
     (assoc db :blups value)))

  (:blups @re-frame.db/app-db)

  (re-frame/dispatch [::test-event nil]))
