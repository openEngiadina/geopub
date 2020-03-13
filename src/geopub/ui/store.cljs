(ns geopub.ui.store
  (:require [rdf.core :as rdf]
            [clojure.core.logic :as l]
            [geopub.ui.utils :refer [rdf-term-component]]))

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
  [:div#store
   [:h1 "Store"]
   [triple-table (rdf/triple-seq
                  (:store @state))]
   ])
