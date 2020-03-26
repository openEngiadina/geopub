(ns geopub.state
  "Helper for managing application state"
  (:require-macros [cljs.core.async :refer [go]])
  (:require [reagent.core :as r]
            [rdf.graph.map]
            [rdf.core :as rdf]
            [cljs.core.async :as async :refer [<!]]))

(defn init []
  (r/atom
   {:graph (rdf.graph.map/graph)}))

(defn add-rdf-graph!
  "Takes RDF graph from a channel and merge with stategraph."
  [state chan]
  (go
    ;; get the input from channel
    (let [input (<! chan)]

      ;; if input is a graph
      (if (rdf/graph? input)

        ;; swap state graph with graph merged with input graph
        (swap! state (fn [state]
                       (assoc state :graph
                              (rdf/graph-merge (:graph state) input))))

        ;; else return whatever input is (probably an error)
        input))))

(defn reset-graph! [state]
  (swap! state #(assoc % :graph (rdf.graph.map/graph))))
