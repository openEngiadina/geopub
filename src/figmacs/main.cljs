
(ns figmacs.main
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [leaflet :as leaflet]
            [react-leaflet :as react-leaflet]
            [clojure.string :as str]
            [cljsjs.moment]
            [figmacs.tours :as tours]))

(def copy-osm "&copy; <a href=\"http://osm.org/copyright\">OpenStreetMap</a> contributors")
(def osm-url "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")

(def Map (r/adapt-react-class react-leaflet/Map))
(def TileLayer (r/adapt-react-class react-leaflet/TileLayer))
(def Marker (r/adapt-react-class react-leaflet/Marker))
(def Popup (r/adapt-react-class react-leaflet/Popup))
(def Polyline (r/adapt-react-class react-leaflet/Polyline))
(def CircleMarker (r/adapt-react-class react-leaflet/CircleMarker))

;; ====================== Config =============================================

(def server-url "http://localhost:8080/")

(def default-center
  ;; Default center of map is Scuol
  [46.8 10.283333])


;; ===========================================================================


;; =================== State and helpers =====================================


(defonce state
  (r/atom {:actor-id (str server-url "actors/alice")
           :actor {}
           :active-page :timeline
           :selected nil
           :hover nil
           :latlng default-center}))

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

(defn get-actor-outbox [state]
  (get-in @state [:actor :outbox]))

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
     [actor-profile (:body (<! (http-get (:actor-id @state))))]

      (swap! state
             #(assoc % :actor actor-profile)))))

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


(defn object-component [activity]
  (let [object (:object activity)]
    [:div.object
     (case (:type object)
       "Note" [:div
               [:h3 "Note"]
               [:p.note-content (:content object)]]

       "discover.swiss/Tour" [tours/tour-component activity]

       "Status" [:div
                 [:h3 "Status"]
                 [tours/status-component activity true]]

       [:dl
        [:dt "type"]
        [:dd (:type object)]])]))

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
     [:span.type (str (:type activity) ": " (:type (:object activity)))]
     [:span.published (.fromNow (js/moment (:published activity)))]]]

   [object-component activity]

   [:details [:code (prn-str activity)]]])

(defn inbox-component [inbox]
  [:div
   [:h2 "Inbox"]
   (map (fn [activity] [activity-component activity]) (:items inbox))])

(defn outbox-component [outbox]
  [:div
   [:h2 "Outbox"]
   (map (fn [activity] [activity-component activity]) (:items outbox))])

(defn post-activity! [activity]
  (http/post (get-actor-outbox state) {:with-credentials? false :json-params activity}))

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
      (nav-element :curated)
      (nav-element :system)]]))

(defn system-page []
  [:div
   [:p "System information and technical details."]

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
              :on-click #(tours/post-sample-tours post-activity!)}]]

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

(defn ap-demo-app []
  (let
   [is-selected? (partial is-selected? state)
    is-hover? (partial is-hover? state)]
    [:div#container

     [:div#sidebar
      [:h1 "GeoPub"]
      [nav-bar (:active-page @state)]

      [:hr]

      (case (:active-page @state)

        :timeline [timeline-page]

        :events "TODO"

        :notes [:div
                [note-input post-activity!]
                [:hr]
                [timeline-page (filter #(= (get-in % [:object :type]) "Note") (get-public-activities state))]]

        :tours [tours/tours-component
                (get-public-activities state)
                is-selected?
                (partial set-selected! state)
                (partial set-hovered! state)
                post-activity!
                (partial tours/tour-status (get-public-activities state))]

        :curated "TODO"

        :system [system-page]

        "404")]

     [:div#mapid
      [Map
       {:style {:min-height "98vh"}
        :center default-center
        :zoom 12
        :on-click (fn [e]
                    (let [latlng (js->clj (.-latlng e))]
                      (set-position! state [(get latlng "lat") (get latlng "lng")])))}
       [TileLayer
        {:url osm-url
         :attribution copy-osm}]

       (for [activity (get-in @state [:public :items])]

         (let [id (:id activity)
               location (object-location (:object activity))]
           (when-not (nil? location)
             [Marker {:position location
                      :id id
                      :on-click #(set-selected! state id)
                      :on-mouse-over #(set-hovered! state id)
                      :on-mouse-out #(set-hovered! state nil)}

              [Popup
               [object-component activity]]

              (when (is-selected? state activity)
                [CircleMarker
                 {:center location
                  :radius 10}])])))

       (for [activity (get-public-activities state)]
         (let
          [id (:id activity)
           tour (:object activity)
           tour-line (tours/tour-line tour)]
           (when (and (tours/tour? tour)
                      tour-line)
             [Polyline {:color (cond
                                 (is-selected? activity) "red"
                                 (is-hover? activity) "red"
                                 :else "blue")
                        :positions tour-line
                        :on-click #(set-selected! state id)
                        :on-mouse-over #(set-hovered! state id)
                        :on-mouse-out #(set-hovered! state nil)}

              [Popup [tours/tour-component activity false (partial tours/tour-status (get-public-activities state))]]])))]]]))

(r/render [ap-demo-app]
          (js/document.getElementById "app")
          refresh!)

(defonce refresher
  (js/setInterval #(refresh!) 2000))

