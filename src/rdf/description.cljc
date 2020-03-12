(ns rdf.description
  "A graph with a starting point.

  Having a starting point allows lookup of like for a map.
  "
  (:require [rdf.core :as rdf]))

(defrecord Description [subject graph])

(defn description [subject graph]
  (->Description subject graph))

(defn description-subject [description]
  (:subject description))
