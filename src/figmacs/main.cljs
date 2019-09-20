(ns figmacs.main
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]))

(def actor-id (r/atom "http://localhost:8080/actors/alice"))
(def actor (r/atom {}))

(defn get-and-reset! [atom url]
  (go (let [response (<! (http/get url {:with-credentials? false}))]
        (reset! atom (:body response)))))

(defn http-get [url]
  (http/get url {:with-credentials? false}))

(defn get-and-reset-actor! [atom actor-id]
  (go
    (let
        [actor-profile (:body (<! (http-get actor-id)))
         actor-inbox (:body (<! (http-get (:inbox actor-profile))))
         actor-outbox (:body (<! (http-get (:outbox actor-profile))))
         ]
      (reset! atom (merge actor-profile {:inbox actor-inbox} {:outbox actor-outbox})))))

(get-and-reset-actor! actor "http://localhost:8080/actors/bob")

(defn post-activity! [activity]
  (println activity)
  (http/post "http://localhost:8080/actors/alice/outbox" {:with-credentials? false :json-params activity}))

(defn actor-component [actor]
  [:div
   [:h2 "Actor"]
   [:dl [:dt "Name"] [:dd (:name actor)]]])

(defn activity-component [activity]
  [:div#activity
   [:dl
    [:dt "id"] [:dd (:id activity)]
    [:dt "type"] [:dd (:type activity)]
    [:dt "to"] [:dd [:code (prn-str (:to activity))]]
    [:dt "object"] [:dd [:code (prn-str (:object activity))]]]])

(defn inbox-component [inbox]
  [:div
   [:h2 "Inbox"]
   (map (fn [activity] [activity-component activity]) (:orderedItems inbox))])

(defn outbox-component [outbox]
  [:div
   [:h2 "Outbox"]
   (map (fn [activity] [activity-component activity]) (:orderedItems outbox))])

(defn create-note [content]
  {:type "Note"
   :content content})

(defn note-input [on-save]
  (let [note-content (r/atom "")

        create-note (fn [content] {:type "Note" :content content})
        create-activity (fn [to object] {:type "Create" :to to :object object})

        stop #(reset! note-content "")
        save #(do (if on-save
                    (->> @note-content
                         create-note
                         (create-activity ["http://localhost:8080/actors/bob"])
                         on-save))
                  (stop))]

    (fn [on-save]
      [:div#create-note
       [:input {:type "text"
                :placeholder "A note to Bob..."
                :value @note-content
                :on-change #(reset! note-content (-> % .-target .-value))
                :on-key-down #(case (.-which %)
                                13 (save)
                                27 (stop)
                                nil)}]])))

(defn ap-demo-app []
  [:div
   [:h1 "AP Demo"]
   [actor-component @actor]
   [inbox-component (:inbox @actor)]
   [outbox-component(:outbox @actor)]
   [note-input post-activity!]
   [:input {:type "button"
            :value "Refresh"
            :on-click #(get-and-reset-actor! actor "http://localhost:8080/actors/alice")}]])

(r/render [ap-demo-app]
          (js/document.getElementById "app"))
