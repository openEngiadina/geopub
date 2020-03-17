(ns rdf.core
  (:require [rdf.core :as rdf]))

(defmacro defns
  "Macro for defining a custom new namespace"
  [prefix namespace]
  (let
      [relative (gensym "relative-")]
      `(defn ~prefix
         [~relative]
         (rdf/iri
          (str ~namespace (name ~relative))))))
