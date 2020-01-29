(ns geopub.ui.utils
  (:require [cljs-rdf.core :as rdf]))

(defn iri-component [iri & {:keys [class]}]
  (let [class (or class "iri")]
    (cond

      (rdf/iri? iri)
      [:span
       {:class class}
       (rdf/iri-value iri)]

      (seq? iri)
      (iri-component (first iri))

      :else
      [:span.iri "-"])))

(defn literal-component [literal]
  (rdf/literal-value literal))
