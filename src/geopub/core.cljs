;; Copyright © 2019, 2020 pukkamustard <pukkamustard@posteo.net>
;;
;; This file is part of GeoPub.
;;
;; GeoPub is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GeoPub is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GeoPub.  If not, see <https://www.gnu.org/licenses/>.

(ns geopub.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [leaflet :as leaflet]
            [react-leaflet :as react-leaflet]
            [clojure.string :as str]
            [cljsjs.moment]
            [geopub.tours :as tours]
            [geopub.ui :as ui]))


;; ====================== Config =============================================

(def server-url "https://ap-dev.miaengiadina.ch/")
;;(def server-url "http://localhost:8080/")


(defonce state
  (r/atom {}))

;; =================== State and helpers =====================================


(defn set-selected! [state selected]
  "Set selected activity/object"
  (swap! state assoc :selected selected))

(defn is-selected? [state thing]
  "Is the thing selected?"
  (or
   (= (:selected @state) thing)
   (= (:selected @state) (:id thing))))

(defn set-hovered! [state hovered]
  "Set the hovered on activity/object"
  (swap! state assoc :hover hovered))

(defn is-hover? [state thing]
  "Is the thing selected?"
  (or
   (= (:hover @state) thing)
   (= (:hover @state) (:id thing))))

(defn set-position! [state pos]
  (swap! state assoc :position pos))

(defn get-position [state]
  (:position @state))

(defn get-public-activities [state]
  (get-in @state [:public :items]))

(defn get-public-objects [state]
  (distinct
   (map :object (get-in @state [:public :items]))))

(defn get-liked-objects [state]
  (get-in @state [:liked :items]))

(defn is-liked? [state object]
  (some (fn [id] (= (:id object) id))
        (map :id (get-liked-objects state))))

(defn get-actor-outbox [state]
  (get-in @state [:actor :outbox]))

(defn post-activity! [activity]
  (http/post (get-actor-outbox state) {:with-credentials? false :json-params activity}))

(defn like-object! [object]
  (post-activity! {:type "Like"
                   :to "https://www.w3.org/ns/activitystreams#Public"
                   :object (or (:id object) object)}))

;; ===========================================================================


(defn http-get [url]
  (http/get url {:with-credentials? false}))

(defn reset-database! []
  (go
    (<! (http-get (str server-url "api/dev/reset")))))

(defn get-actor! []
  ;; Get the ActivityPub Actor along with inbox/outbox
  (go
    (let
     [actor-profile (:body (<! (http-get (:actor-id @state))))
      liked (:body (<! (http-get (:liked actor-profile))))]

      (swap! state
             #(assoc % :actor actor-profile))

      (swap! state
             #(assoc % :liked liked)))))

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


(defn object-component [object is-liked? like-object!]
  [:div.object
   (case (:type object)
     "Note" [:div
             [:h3 "Note"]
             [:p.note-content (:content object)]]

     "discover.swiss/Tour" [tours/tour-component object]

     "Status" [:div
               [:h3 "Status"]
               [tours/status-component object true]]

     [:dl
      [:dt "type"]
      [:dd (or (get object (keyword "@type")) (:type object))]
      [:dt "name"]
      [:dd (:name object)]
      ])

   (when (and is-liked? like-object!)
     (if (is-liked? object)
       [:button.like {:disabled true} "Liked!"]
       [:button.like {:on-click #(like-object! object)} "♥"]))])

(defn activity-component [activity]
  [:div.activity
   {:class (if (is-selected? state activity)
             ["selected"]
             [])
    :on-click #(set-selected! state (:id activity))
    :on-mouse-over #(set-hovered! state (:id activity))
    :on-mouse-out #(set-hovered! state nil)}

   [:div.meta
    [:p
     [:span.actor (:actor activity)]
     (case (:type activity)
       "Create" (str " created a " (:type (:object activity)))
       "Like" (str " liked " (:id (:object activity)))
       (str " " (:type activity) " "))
     [:span.published (.fromNow (js/moment (:published activity)))]]]

   (when
    (= (:type activity) "Create")
     [object-component (:object activity) (partial is-liked? state) like-object!])

   [:details [:code (prn-str activity)]]
])

(defn inbox-component [inbox]
  [:div
   [:h2 "Inbox"]
   (map (fn [activity] [activity-component activity]) (:items inbox))])

(defn outbox-component [outbox]
  [:div
   [:h2 "Outbox"]
   (map (fn [activity] [activity-component activity]) (:items outbox))])

(defn note-input [on-save]
  (let [note-content (r/atom "")

        create-note (fn [content] {:type "Note" :content content})
        add-location (fn [object] (if-not (nil? (:latlng @state))
                                    (merge object
                                           {:location
                                            {"@type" "Place"
                                             :geo {"@type" "GeoCoordinates"
                                                   :latitude (first (get-position state))
                                                   :longitude (second (get-position state))}}})
                                    object))

        create-activity (fn [to object] {:type "Create" :to to :object object})

        stop #(reset! note-content "")
        save #(do (if on-save
                    (->> @note-content
                         create-note
                         add-location
                         (create-activity ["https://www.w3.org/ns/activitystreams#Public"])
                         on-save))
                  (stop))]

    (fn [on-save]
      [:div#create-note
       [:h3 "Create a Note"]
       [:input.text-input {:type "text"
                           :placeholder "A note ..."
                           :value @note-content
                           :on-change #(reset! note-content (-> % .-target .-value))
                           :on-key-down #(case (.-which %)
                                           13 (save)
                                           27 (stop)
                                           nil)}]
       [:p.latlng (str "Position: "
                       (get-position state)
                       " (Click on map to update)")]

       [:input.send-button {:type "button" :value "Create" :on-click save}]])))

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
      ;; (nav-element :events)
      (nav-element :notes)
      (nav-element :tours)
      (nav-element :liked)
      (nav-element :system)]]))

(defn system-page []
  [:div
   [:h2 "Actor"]

   [:dl
    [:dt "ID"] [:dd (get-in @state [:actor-id])]
    [:dt "Name"] [:dd (get-in @state [:actor :name])]
    [:dt "Inbox"] [:dd (get-in @state [:actor :inbox])]
    [:dt "Outbox"] [:dd (get-in @state [:actor :outbox])]]

   ;; [inbox-component (get-in @state [:actor :inbox])]
   ;; [outbox-component (get-in @state [:actor :outbox])]

   [:h2 "Actions"]

   ;; [:input {:type "button"
   ;;          :value "Refresh"
   ;;          :on-click #(refresh!)}]

   [:ul {:class "actions-list"}

    [:li
     [:input {:type "button"
              :value "Login as Alice"
              :on-click #(do
                           (swap! state assoc :actor-id (str server-url "actors/alice"))
                           (refresh!))}]]

    [:li
     [:input {:type "button"
              :value "Login as Bob"
              :on-click #(do
                           (swap! state assoc :actor-id (str server-url "actors/bob"))
                           (refresh!))}]]

    [:li
     [:input {:type "button"
              :value "Import sample tours"
              :on-click #(tours/post-sample-data post-activity! "ds-tours.json")}]]

    [:li
     [:input {:type "button"
              :value "Import Ski lifts & pistes from OpenStreetMaps"
              :on-click #(tours/post-sample-data post-activity! "scuol-ski.json")}]]

    [:li
     [:input {:type "button"
              :value "Reset Database"
              :on-click #(reset-database!)}]]]])

(defn timeline-page [activities]
  [:div#timeline
   (map (fn [activity]
          [:div
           [activity-component activity]
           [:hr]])
        (if activities
          activities
          (get-public-activities state)))])

(defn object-location [object]
  (when-not (or (nil? (get-in object [:location :geo]))
                (nil? (get-in object [:location :geo :latitude]))
                (nil? (get-in object [:location :geo :longitude])))
    [(get-in object [:location :geo :latitude])
     (get-in object [:location :geo :longitude])]))


(r/render [ui/ui]
          (js/document.getElementById "app")
          refresh!)

(defonce refresher
  (js/setInterval #(refresh!) 2000))

