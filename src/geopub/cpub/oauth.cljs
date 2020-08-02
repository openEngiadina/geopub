(ns geopub.cpub.oauth
  "Authorization with CPub using OAuth"
  (:require [re-frame.core :as re-frame]
            [reitit.frontend.easy :as rfe]
            
            [ajax.core :as ajax]

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

(re-frame/reg-sub
 ::state
 (fn [db] (get-in db [:oauth :state])))

(re-frame/reg-sub
 ::userinfo
 (fn [db] (get-in db [:oauth :state :userinfo])))

(re-frame/reg-sub
 ::access-token-header
 (fn [db]
   (str "Bearer " (get-in db [:state :token :access_token]))))

(re-frame/reg-event-db
 ::reset-state
 (local-storage/persist :oauth)
 (fn [db _] (dissoc db :oauth)))

(comment
  (re-frame/dispatch [::reset-state]))

(comment
  (re-frame/dispatch [::initialize])
  (:oauth @re-frame.db/app-db))

(re-frame/reg-event-fx
 ::initialize
 [(local-storage/persist :oauth)]
 (fn [coeffects _]
   ;; load db state from local storage
   {:db (-> (get coeffects :db)
            (assoc :oauth (get-in coeffects [:db :oauth])))}))


(defn redirect-uri []
  (str (.-origin js/location)
       (rfe/href :geopub.app.settings/oauth-callback)))

;; Client registration

(re-frame/reg-event-fx
 ::register-client
 (re-frame/path :oauth)
 (fn [coeffects [_ server-url opts]]
   {::http-post [(str server-url "/oauth/clients")
                 {:with-credentials? false
                  :json-params {"client_name" "GeoPub"
                                "scope" ["openid" "read" "write"]
                                "redirect_uris" [(redirect-uri)]}
                  :on-response [::register-client-response server-url opts]}]
    :db (-> (:db coeffects)
            (assoc :state :registering-client))}))

(re-frame/reg-event-fx
 ::register-client-response
 [(local-storage/persist :oauth)
  (re-frame/path :oauth)]
 (fn [coeffects [_ server-url opts response]]
   (if (and (:success response) (= (:status response) 201))

     ;; store client in db and dispatch callback
     {:db (-> (:db coeffects)
              (assoc :state :client-registered)
              (assoc-in [:clients server-url] (:body response)))
      :dispatch (:callback opts)}

     ;; store error in db
     {:db (-> (:db coeffects)
              (assoc :state {:error :client-registration-failed}))})))

(comment
  (re-frame/dispatch [::register-client "http://localhost:4000/"]))

;; Launch an Authorization request

(defn- authorize-url [server-url client state]
  (-> (uri/parse server-url)
      (.setPath "/oauth/authorize")
      (.setQuery (str "response_type=code"
                      "&client_id=" (:client_id client)
                      "&state=" state))
      (.toString)))

(re-frame/reg-event-fx
 ::request-authorization
 [(local-storage/persist :oauth)
  (re-frame/path :oauth)]
 (fn [coeffects [_ server-url]]
   (let [client (get-in coeffects [:db :clients server-url])]
     (cond
       ;; if there is not client in local storage for given server-url, attempt to register a client
       (nil? client) {:dispatch [::register-client server-url
                                 ;; after successful client registration retry
                                 {:callback [::request-authorization server-url]}]}

       ;; Assume there is a valid client and launch OAuth 2.0 authorizaiton request
       :else (let [random-state (goog.string/getRandomString)]
               {:db (-> (:db coeffects)
                        (assoc :current-auth-request {:server-url server-url
                                                      :state random-state})
                        (assoc :state :external-authorization-request))
                :dispatch [:geopub.router/navigate-external
                           (authorize-url server-url client random-state)]})))))


(comment
  (re-frame/dispatch [::request-authorization "http://localhost:4000/" {:client_id "4ae60a48-bfba-41a0-b0a6-0efb46129ab0"}]))

;; Handle callback

(defn token-url [server-url]
  (-> (uri/parse server-url)
      (.setPath "/oauth/token")
      (.toString)))

(re-frame/reg-event-fx
 ::handle-callback
 [(local-storage/persist :oauth)
  (re-frame/path :oauth)]
 (fn [coeffects [_ params opts]]
   (let [db (:db coeffects)
         current-auth-request (:current-auth-request db)
         server-url (:server-url current-auth-request)
         client (get-in db [:clients server-url])]

     (if (= (:state current-auth-request) (:state params))
       ;; state matches what we stored, callback is legit -> get access token
       {:db (-> (:db coeffects)
                (assoc :state :access-token-request)
                (dissoc :current-oauth-request))
        ::http-post [(token-url server-url)
                     {:with-credentials? false
                      :form-params {:grant_type "authorization_code"
                                    :code (:code params)
                                    :redirect_uri (redirect-uri)
                                    :client_id (:client_id client)
                                    ;; authenticate the client by providing the client secret
                                    :client_secret (:client_secret client)}
                      :on-response [::access-token-response server-url]}]}

       ;; store error in db
       {:db (-> (:db coeffects)
                (assoc :state {:error :invalid-authorization-code
                               :params params})
                (dissoc :current-oauth-request))}))))

(re-frame/reg-event-fx
 ::access-token-response
 [(local-storage/persist :oauth)
  (re-frame/path :oauth)]
 (fn [coeffects [_ server-url response]]
   (print response)
   (if (:success response)
     {:db (-> (:db coeffects)
              (assoc :state {:authorized server-url
                             :date (js/Date)
                             :token (:body response)}))
      :dispatch [::get-userinfo]}

     ;; store error
     {:db (-> (:db coeffects)
              (assoc :state {:error :access-token-request-failed
                             :response response}))})))


;; Get userinfo

(defn userinfo-url [server-url]
  (-> (uri/parse server-url)
      (.setPath "/oauth/userinfo")
      (.toString)))

(re-frame/reg-event-fx
 ::get-userinfo
 (re-frame/path :oauth)
 (fn [coeffects [_]]
   (when-let [server-url (get-in coeffects [:db :state :authorized])]
     {:http-xhrio {:method :get
                   :uri (userinfo-url server-url)
                   :response-format (ajax/json-response-format {:keywords? true})
                   :headers {"Authorization"
                             (str "Bearer " (get-in coeffects [:db :state :token :access_token]))}
                   :on-success [::get-userinfo-success]
                   :on-failure [::get-userinfo-failure]}})))

(re-frame/reg-event-fx
 ::get-userinfo-success
 (re-frame/path :oauth)
 (fn [coeffects [_ result]]
   {:db (assoc-in (:db coeffects) [:state :userinfo] result)}))

(re-frame/reg-event-fx
 ::get-userinfo-failure
 (re-frame/path :oauth)
 (fn [coeffects [_ result]]
   {:db (assoc-in (:db coeffects) [:state :userinfo] :error)}))

(defn get-userinfo-component []
  (let [state (re-frame/subscribe [::state])]
    (when (:authorized @state)
      (re-frame/dispatch [::get-userinfo]))))

(comment
  (re-frame/dispatch [::get-userinfo])
  (get-in @re-frame.db/app-db [:oauth :state :userinfo]))
