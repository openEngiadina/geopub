(ns rdf.description
  "A graph with a starting point.

  Having a starting point allows lookup of like for a map.
  "
  (:require-macros [clojure.core.logic :refer [run*]])
  (:require [rdf.core :as rdf]
            [rdf.logic :as rl]
            [clojure.core.logic :as l]))

(defrecord Description [subject graph]

  rdf/ITripleSeq
  (triple-seq [description]
    (run* [t]
      (rl/graph-collecto (:graph description) 1 (:subject description) t))))

(defn description [subject graph]
  (->Description subject graph))

(defn description-subject [description]
  (:subject description))

(defn description-graph [description]
  (:graph description))

(defn description-move [description new-subject]
  (->Description new-subject (:graph description)))

(defn description-tripleo
  [desc p o]
  (rl/graph-tripleo (description-graph desc)
                 (rdf/triple (description-subject desc) p o)))

(defn description-get [description key]
  (run* [o]
    (description-tripleo description key o)))

(defn description-predications [description]
  (run* [p o]
    (description-tripleo description p o)))

