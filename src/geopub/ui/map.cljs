(ns geopub.ui.map
  (:require-macros [cljs.core.logic :refer [run* fresh]])
  (:require
   [reagent.core :as r]
   [leaflet :as leaflet]
   [react-leaflet :as react-leaflet]
   [geopub.ns :refer [geo]]
   [rdf.core :as rdf]
   [rdf.logic :as rl]
   [rdf.description :as rd]
   [cljs.core.logic :as l]))


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

(defn get-geo-object [state]
  "Returns a list of activities that have a latitude and longitude"
  ;; TODO only store the relevant subgraph in the description
  (map #(rd/description % (:store @state))
       (run* [s]
         (fresh [x y]
           (rl/graph-tripleo (:store @state) (rdf/triple s (geo "long") x))
           (rl/graph-tripleo (:store @state) (rdf/triple s (geo "lat") y))))))

(defn get-location [object]
  (let
      [lat (-> object
               (rd/description-get (geo "lat"))
               (first)
               (rdf/literal-value))

       long (-> object
                (rd/description-get (geo "long"))
                (first)
                (rdf/literal-value))]
    [lat long]))

(defn geo-object-component [object]
  [Marker {:position (get-location object)}])

(defn view [state]
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

    (for [geo-object (get-geo-object state)]
      ^{:key (prn-str (rd/description-subject geo-object))}
      [geo-object-component geo-object])

    ]])
