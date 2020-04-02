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

(defn merge-graphs
  "Returns a function that will merge in graphs into state graph. Useful when using swap! on the state."
  [& graphs]
  (fn [state]
    (assoc state :graph
           (reduce rdf/graph-merge
                   (or (:graph state) (rdf.graph.map/graph))
                   graphs))))

(defn add-rdf-graph!
  "Takes RDF graph from a channel and merge with stategraph."
  [state chan]
  (go
    ;; get the input from channel
    (let [input (<! chan)]

      ;; if input is a graph
      (if (rdf/graph? input)

        ;; swap state graph with graph merged with input graph
        (swap! state (merge-graphs input))

        ;; else return whatever input is (probably an error)
        input))))

(defn reset-graph! [state]
  (swap! state #(assoc % :graph (rdf.graph.map/graph))))

(defn logout! [state]
  (swap! state #(dissoc % :account)))
