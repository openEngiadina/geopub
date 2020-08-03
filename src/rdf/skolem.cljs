(ns rdf.skolem
  "Skolemize a RDF Graph."
  (:require [rdf.core :as rdf]
            [rdf.graph.map]))

(defn random-uuid-urn []
  (rdf/iri
   (str "urn:uuid:" (random-uuid))))

(defn- skolem-function [mapping bnode]
  (if-let [t (get @mapping bnode)]
    t
    (let [new-term (random-uuid-urn)]
      (vswap! mapping assoc bnode new-term)
      new-term)))

(defn skolemize []
  "Returns a transducer that skolemizes blank nodes in triples with new random UUID URNs."
  (fn [xf]
    (let [mapping (volatile! {})]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]

         (if (rdf/triple? input)
           
           (let [s (rdf/triple-subject input)
                 p (rdf/triple-predicate input)
                 o (rdf/triple-object input)]

             (xf result
                 (rdf/triple
                  ;; replace blank node subjects with skolem term
                  (if (rdf/blank-node? s) (skolem-function mapping s) s)
                  ;; blank nodes can not appear in predicate position
                  p
                  ;; replace blank node object with skolem term
                  (if (rdf/blank-node? o) (skolem-function mapping o) o))))

           ;; if input is not a triple just pass trough
           (xf result input)))))))

(comment
  (into [] (skolemize) 
        (list (rdf/triple "a" "b" "c")
              (rdf/triple (rdf/blank-node "a") "d" 5)
              (rdf/triple "a" "f" (rdf/blank-node "a"))))

  (transduce (comp (map inc)
                   (skolemize))
             conj #{} (range 5))


  (transduce
   (skolemize)
   (fn [graph triple]
     (if (rdf/triple? triple)
       (rdf/graph-add graph triple)
       graph))
   (rdf.graph.map/graph)
   (-> (rdf.graph.map/graph)
       (rdf/graph-add (rdf/triple (rdf/blank-node "a") "b" 3))
       (rdf/graph-add (rdf/triple "a" "c" (rdf/blank-node "test")))
       (rdf/graph-add (rdf/triple "b" "c" (rdf/blank-node "test")))
       (rdf/graph-add (rdf/triple "d" "c" (rdf/blank-node "test")))
       (rdf/triple-seq))))
