;; Copyright © 2020 pukkamustard <pukkamustard@posteo.net>
;;
;; This file is part of GeoPub.
;;
;; GeoPub is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GeoPub is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GeoPub.  If not, see <https://www.gnu.org/licenses/>.

(ns cljs-rdf.description
  "A set of triples about the same subject (idea stolen from RDF.ex)"
  (:require [cljs-rdf.core :as rdf]
            [cljs.core.logic :refer [lvar]]))

;; TODO store as hashmap from predicate to objects and increase lookup speed

;; TODO store prefixes that would allow lookups such as (get :type description) that would expand to (get (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")). Allow custom prefixes such that (get :object description) may refer to the ActivityStreams object field. This would allow very easy access to RDF data (similar to LDFlex).

;; NOTE both todos above should probably done on a more efficient graph type. A description would then just be a slice of a graph.

(defrecord Description [subject triples])

(defn description-get
  ([desc key]
   (->> (:triples desc)
        (filter #(= key (rdf/triple-predicate %)))
        (map rdf/triple-object)))
  ([desc key not-found]
   (or (-lookup desc key) not-found)))

(defn description [subject triples]
  (cond

    (rdf/graph? triples)
    (->Description subject
                   (rdf/graph-match triples (rdf/triple subject (lvar) (lvar))))

    (seq? triples)
    (->Description subject
                   (filter #(= subject (rdf/triple-subject %))


                           triples))))


