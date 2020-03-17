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

(defn add-triples!
  "Takes triples from a channel and adds them to the graph. The input channel must be closed before changes are applied."
  [state chan]
  (go
    (let [input-graph (<! (async/reduce
                           ;; read sequence of triples and add to graph
                           (fn [graph input]
                             (reduce rdf/graph-add graph (rdf/triple-seq input)))
                           ;; initialize a fresh graph
                           (rdf.graph.map/graph)
                           chan))
          ;; merge graph in state with input-graph
          new-graph (rdf/graph-merge (:graph @state) input-graph)]
      ;; swap in new graph
      (swap! state (fn [state]
                     (assoc state :graph new-graph))))))

(defn reset-graph! [state]
  (swap! state #(assoc % :graph (rdf.graph.map/graph))))
