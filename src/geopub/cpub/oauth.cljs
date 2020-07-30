(ns geopub.cpub.oauth
  "Authorization with CPub using OAuth"
  (:require [re-frame.core :as re-frame]
            [reitit.frontend.easy :as rfe]

            [cljs.core.async :as async :refer [<!]]
            [cljs-http.client :as http]
            [goog.Uri :as uri]

            [geopub.local-storage :as local-storage])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [async-error.core :refer [<? go-try]]))

(re-frame/reg-fx
 ::http-post
 (fn [args]
   (go (let [[url opts] args
             response (<! (http/post url opts))]
         (when-let [on-response (:on-response opts)]
           (re-frame/dispatch (conj on-response response)))))))

(defn redirect-uri []
  (str (.-origin js/location)
       (rfe/href :geopub.app.settings/oauth-callback)))

(re-frame/reg-sub
 ::client
 (fn [db [_ server-url]]
   (get-in db [:oauth-clients server-url])))

(re-frame/reg-event-fx
 ::register-client
 (fn [coeffects [_ server-url opts]]
   {::http-post [(str server-url "/oauth/clients")
                 {:with-credentials? false
                  :json-params {"client_name" "GeoPub"
                                "scope" ["openid" "read" "write"]
                                "redirect_uris" [(redirect-uri)]}
                  :on-response [::register-client-response server-url opts]}]
    :db (assoc-in (:db coeffects) [:oauth-clients server-url] :loading)}))

(re-frame/reg-event-fx
 ::register-client-response
 (local-storage/persist :oauth-clients)
 (fn [coeffects [_ server-url opts response]]
   (let [
         client-or-error (if (and (:success response) (= (:status response) 201))
                           (:body response)
                           :error)
         db-effect {:db (assoc-in (:db coeffects) [:oauth-clients server-url]
                                  client-or-eror)}
         dispatch-effect (when (:callback opts)
                           {:dispatch (:callback opts)})])
   (merge db-effects dispatch-effects)))

(comment
  (re-frame/dispatch [::register-client "http://localhost:4000/"]))


(comment
  (defn register-client! [server-url]
    "Registers a new client on the CPub instance"
    (go (let [response (<! (http/post
                            (clojure.string/join [server-url "/oauth/clients"])
                            {:with-credentials? false
                             :json-params {"client_name" "GeoPub"
                                           "scope" ["openid" "read" "write"]
                                           "redirect_uris" [(redirect-uri)]
                                           }}))]
          (if (== (:status response) 201)
            (:body response)))))

  (defn retrieve-persisted-clients! []
    (or (cljs.reader/read-string
         (js/window.localStorage.getItem "geopub-oauth-clients"))
        {}))


  (defn persist-client! [server-url client]
    "Persist client in local storage"
    (js/window.localStorage.setItem "geopub-oauth-clients"
                                    (prn-str
                                     (assoc
                                      (retrieve-persisted-clients!)
                                      server-url client)))
    ;; return the client
    client)

  (defn get-or-register-client! [server-url]
    (go
      (if-let [client
               (get (retrieve-persisted-clients!) server-url)]
        client
        (->> (<! (register-client! server-url))
             (persist-client! server-url)))))


  (defn authorize! [state server-url]
    (go-try
     (let [client (<! (get-or-register-client! server-url))
           authorize-uri (->
                          (uri/parse server-url)
                          (.setPath "/oauth/authorize")
                          (.setQuery (str "response_type=code&"
                                          "client_id=" (:client_id client)))
                          (.toString))]
       (print authorize-uri))))

  ;; (authorize! {} "http://localhost:4000/")

  )
