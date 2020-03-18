(ns geopub.ui.store
  (:require [rdf.core :as rdf]
            [clojure.core.logic :as l]
            [geopub.data.rdf :refer [rdf-term-component]]))

(defn triple-table [triples]
  [:table
   [:tbody
    (for [t triples]
      ^{:key (prn-str t)}
      [:tr
       [:td [rdf-term-component (rdf/triple-subject t)]]
       [:td [rdf-term-component (rdf/triple-predicate t)]]
       [:td [rdf-term-component (rdf/triple-object t)]]])
    ]])

(defn view [state]
  [:div.ui-page
   [:main
    [:h1 "Store"]
    [triple-table (rdf/triple-seq
                   (:graph @state))]]])
