(ns geopub.ui.store
  (:require [cljs-rdf.core :as rdf]))

(defn triple-table [triples]
  [:table
   [:tbody
    (for [t triples]
      ^{:key (prn-str t)}
      [:tr
       [:td (prn-str (rdf/triple-subject t))]
       [:td (prn-str (rdf/triple-predicate t))]
       [:td (prn-str (rdf/triple-object t))]])
    ]])

(defn view [state]
  [:div#store
   [:h1 "Store"]
   [:p (str (count (:store @state)) " triples loaded.")]
   [triple-table (:store @state)]
   ;; [triple-table (take 1 (:store @state))]
   ])
