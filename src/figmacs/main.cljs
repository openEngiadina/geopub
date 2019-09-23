(ns figmacs.main
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [leaflet :as leaflet]
            [react-leaflet :as react-leaflet]
            [clojure.string :as str]
            [cljsjs.moment]))

(def copy-osm "&copy; <a href=\"http://osm.org/copyright\">OpenStreetMap</a> contributors")
(def osm-url "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")

(def Map (r/adapt-react-class react-leaflet/Map))
(def TileLayer (r/adapt-react-class react-leaflet/TileLayer))

(def server-url "http://localhost:8080/")

(defonce state
  (r/atom {:actor-id (str server-url "actors/alice")
           :actor {}
           :active-page :timeline}))

(defn http-get [url]
  (http/get url {:with-credentials? false}))

(defn get-actor! []
  ;; Get the ActivityPub Actor along with inbox/outbox
  (go
    (let
     [actor-profile (:body (<! (http-get (:actor-id @state))))
      actor-inbox (:body (<! (http-get (:inbox actor-profile))))
      actor-outbox (:body (<! (http-get (:outbox actor-profile))))]

      (swap! state
             #(assoc % :actor (merge actor-profile
                                     {:inbox actor-inbox
                                      :outbox actor-outbox}))))))

(defn get-public! []
  ;; Get the public collection
  (go
    (let
     [public (:body (<! (http-get (str server-url "public"))))]
      (swap! state #(assoc % :public public)))))

(defn refresh! []
  ;; Refresh data from server (Actor and public collection)
  (get-actor!)
  (get-public!))


;; -- Components -----------------------------------------------------------------------------------------------


(defn object-component [object]
  [:div.object
   (case (:type object)
     "Note" [:p.note-content (:content object)]

     ;; If don't know how to display, show the plain object
     [:code (prn-str object)])])

(defn activity-component [activity]
  [:div.activity
   [:div.meta
    [:p 
     [:span.actor (:actor activity)]
     [:span.published (.fromNow (js/moment (:published activity)))]]]
   [object-component (:object activity)]])

(defn inbox-component [inbox]
  [:div
   [:h2 "Inbox"]
   (map (fn [activity] [activity-component activity]) (:items inbox))])

(defn outbox-component [outbox]
  [:div
   [:h2 "Outbox"]
   (map (fn [activity] [activity-component activity]) (:items outbox))])

(defn post-activity! [activity]
  (println activity)
  (http/post "http://localhost:8080/actors/alice/outbox" {:with-credentials? false :json-params activity}))

(defn note-input [on-save]
  (let [note-content (r/atom "")

        create-note (fn [content] {:type "Note" :content content})
        create-activity (fn [to object] {:type "Create" :to to :object object})

        stop #(reset! note-content "")
        save #(do (if on-save
                    (->> @note-content
                         create-note
                         (create-activity ["https://www.w3.org/ns/activitystreams#Public"])
                         on-save))
                  (stop))]

    (fn [on-save]
      [:div#create-note
       [:input#text-input {:type "text"
                           :placeholder "A note ..."
                           :value @note-content
                           :on-change #(reset! note-content (-> % .-target .-value))
                           :on-key-down #(case (.-which %)
                                           13 (save)
                                           27 (stop)
                                           nil)}]
       [:input#send-button {:type "button" :value "Send" :on-click save}]])))

(defn nav-bar [active-page]
  (let
   [nav-element (fn [page]
                  [:li [:a
                        {:href (str "#" (name page))
                         :class (if (= page active-page) ["active"] [])
                         :on-click (fn [_] (swap! state #(assoc % :active-page page)))}
                        (str/capitalize (name page))]])]
    [:nav
     [:ul
      (nav-element :timeline)
      (nav-element :events)
      (nav-element :tours)
      (nav-element :system)]]))

(defn system-page []
  [:div
   [:p "System information and technical details."]

   [:h2 "Actor"]

   [:dl
    [:dt "ID"] [:dd (get-in @state [:actor-id])]
    [:dt "Name"] [:dd (get-in @state [:actor :name])]]

   ;; [inbox-component (get-in @state [:actor :inbox])]
   ;; [outbox-component (get-in @state [:actor :outbox])]

   [:h2 "Actions"]

   [:input {:type "button"
            :value "Refresh"
            :on-click #(refresh!)}]])

(defn timeline-page []
  [:div#timeline
   [note-input post-activity!]
   [:hr]
   (map (fn [activity]
          [:div
           [activity-component activity]
           [:hr]])
        (get-in @state [:public :items]))])

(def default-center
  ;; Default center of map is Scuol
  [46.8, 10.283333])

(defn ap-demo-app []
  [:div#container

   [:div#sidebar
    [:h1 "GeoPub"]
    [nav-bar (:active-page @state)]

    [:hr]

    (case (:active-page @state)

      :timeline [timeline-page]

      :events "TODO"

      :tours "TODO"

      :system [system-page])]

   [:div#mapid
    [Map
     {:style {:min-height "98vh"}
      :center default-center
      :zoom 12
      :ref (fn [el] (prn el))
      :on-click (fn [el] (prn el))}
     [TileLayer
      {:url osm-url
       :attribution copy-osm}]]]])

(r/render [ap-demo-app]
          (js/document.getElementById "app")
          refresh!)

(defonce refresher
  (js/setInterval #(get-public!) 2000))

