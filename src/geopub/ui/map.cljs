(ns geopub.ui.map
  (:require
   [reagent.core :as r]
   [leaflet :as leaflet]
   [react-leaflet :as react-leaflet]))


(def copy-osm "&copy; <a href=\"http://osm.org/copyright\">OpenStreetMap</a> contributors")
(def osm-url "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")

(def Map (r/adapt-react-class react-leaflet/Map))
(def TileLayer (r/adapt-react-class react-leaflet/TileLayer))
(def Marker (r/adapt-react-class react-leaflet/Marker))
(def Popup (r/adapt-react-class react-leaflet/Popup))
(def Polyline (r/adapt-react-class react-leaflet/Polyline))
(def CircleMarker (r/adapt-react-class react-leaflet/CircleMarker))

(def default-center
  ;; Default center of map is Scuol
  [46.8 10.283333])

(defn set-position! [state pos]
  (swap! state assoc :position pos))

(defn map-component [state]
  [:div#map
   [Map
    {:style {:min-height "100vh"}
     :center default-center
     :zoom 12
     :on-click (fn [e]
                 (let [latlng (js->clj (.-latlng e))]
                   (set-position! state [(get latlng "lat") (get latlng "lng")])))}

    [TileLayer
     {:url osm-url
      :attribution copy-osm}]

    ]])
