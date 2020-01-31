(ns geopub.ui.store
  (:require [cljs-rdf.core :as rdf]
            [cljs.core.logic :as l]
            [geopub.ui.utils :refer [rdf-term-component]]))

;; TODO this should probably go in Graph protocol
(defn all-triples [graph]
  (rdf/graph-match graph (rdf/triple (l/lvar) (l/lvar) (l/lvar))))

(defn triple-table [graph]
  [:table
   [:tbody
    (for [t (all-triples graph)]
      ^{:key (prn-str t)}
      [:tr
       [:td [rdf-term-component (rdf/triple-subject t)]]
       [:td [rdf-term-component (rdf/triple-predicate t)]]
       [:td [rdf-term-component (rdf/triple-object t)]]])
    ]])

(defn view [state]
  [:div#store
   [:h1 "Store"]
   [triple-table (:store @state)]
   ])
