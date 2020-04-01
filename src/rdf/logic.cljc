(ns rdf.logic
  "Use core.logic with RDF!"
  (:require-macros [clojure.core.logic :refer [run* fresh]])
  (:require [rdf.core :as rdf]
            [rdf.ns :as ns]
            [clojure.core.logic :as l]))


;; TODO: program against the ILiteral, IIRI, etc. protocols and not the types!

(extend-type rdf/IRI
  l/IUnifyTerms
  (l/-unify-terms [u v s]
    (if (instance? rdf/IRI v)
      (l/unify s (:value u) (:value v))
      (l/unify s v u)))

  l/IWalkTerm
  (l/-walk-term [v s]
    (rdf/->IRI
     (l/-walk* s (:value v))))

  l/IReifyTerm
  (l/-reify-term [v s]
    (l/-reify* s (:value v))))

(extend-type rdf/BlankNode
  l/IUnifyTerms
  (l/-unify-terms [u v s]
    (if (instance? rdf/BlankNode v)
      (l/unify s (:id u) (:id v))
      (l/unify s v u)))

  l/IWalkTerm
  (l/-walk-term [v s]
    (rdf/->BlankNode
     (l/-walk* s (:id v))))

  l/IReifyTerm
  (l/-reify-term [v s]
    (l/-reify* s (:id v))))

(extend-type rdf/Literal
  l/IUnifyTerms
  (-unify-terms [u v s]
    (if (instance? rdf/Literal v)
      (-> s
          (l/unify (:value u) (:value v))
          (l/unify (:language u) (:language v))
          (l/unify (:datatype u) (:datatype v)))
      (l/unify s v u)))

  l/IWalkTerm
  (l/-walk-term [v s]
    (rdf/->Literal
     (l/-walk* s (:value v))
     (l/-walk* s (:language v))
     (l/-walk* s (:datatype v))))

  l/IReifyTerm
  (l/-reify-term [v s]
    (-> s
        (l/-reify* (:value v))
        (l/-reify* (:language v))
        (l/-reify* (:datatype v)))))

(extend-type rdf/Triple
  l/IUnifyTerms
  (l/-unify-terms [u v s]
    (if (instance? rdf/Triple v)
      (-> s
          (l/unify (:subject u) (:subject v))
          (l/unify (:predicate u) (:predicate v))
          (l/unify (:object u) (:object v)))
      (l/unify s v u)))

  l/IWalkTerm
  (l/-walk-term [v s]
    (rdf/->Triple
     (l/-walk* s (:subject v))
     (l/-walk* s (:predicate v))
     (l/-walk* s (:object v))))

  l/IReifyTerm
  (l/-reify-term [v s]
    (-> s
        (l/-reify* (:subject v))
        (l/-reify* (:predicate v))
        (l/-reify* (:object v)))))

;; Basic Triple relations.

(defn tripleo
  "Relation to match triple"
  [s p o t]
  (fn [a]
    (l/unify a (rdf/triple s p o) t)))


(defn triple-subjecto
  "Relation between triple and subject"
  [t s]
  (l/fresh [p o]
    (tripleo s p o t)))

(defn triple-predicateo
  "Relation between triple and predicate"
  [t p]
  (l/fresh [s o]
    (tripleo s p o t)))

(defn triple-objecto
  "Relation between triple and object"
  [t o]
  (l/fresh [s p]
           (tripleo s p o t)))


;; Graph

(defn graph-tripleo
  "Relation to match triples in a graph"
  ([graph q]
   (fn [a]
     (let [q (l/-walk* a q)]
       (cond
         (rdf/triple? q)
         (l/to-stream
          (map #(l/unify a % q)
               (rdf/graph-match graph q)))

         ;; dump all triples
         (l/lvar? q)
         (l/to-stream (map #(l/unify a % q)
                           (rdf/graph-match graph
                                            (rdf/triple (l/lvar) (l/lvar) (l/lvar)))))

         ;; will not match with anything else
         :else (l/to-stream '())))))
  ([graph s p o]
   (graph-tripleo graph (rdf/triple s p o))))

;; RDFS
;; See "Simple and efficient minimal RDFS" -  Muñoz, J Pérez, C Gutierrez - Journal of web semantics, 2009
 
(defn- rdfs-subclass-ao
  "Rule (3) (a)"
  [graph a c]
  (fresh [b]
    (l/conda
     ;; a is directly subClassOf c
     [(graph-tripleo graph a (ns/rdfs "subClassOf") c)]
     ;; b is directly subClass of c
     [(graph-tripleo graph b (ns/rdfs "subClassOf") c)
      ;; and a is subclass of b
      (rdfs-subclass-ao graph a b)]
     ;; cond requires at least one non-failing line. TODO: why?
     [l/s# l/u#])))

(defn- rdfs-subclass-bo
  "Rule (3) (b)"
  [graph x b]
  (fresh [a]
    (rdfs-subclass-ao graph a b)
    (graph-tripleo graph x (ns/rdf "type") a)))

(defn graph-typeo
  "Relation between subject and type using rdf:type. Implements subclassing."
  [graph s rdf-type]
  (l/conde
   [(graph-tripleo graph s (ns/rdf "type") rdf-type)]
   [(rdfs-subclass-bo graph s rdf-type)]))

(defn reachableo
  "Is z reachable from a in graph withing n steps?"
  [graph n a z]
  (if (> n 0)
    (fresh [t1 t2 via]
           (l/conde

       ;; base case: there is a triple t1 directly connecting a to z
            [(triple-subjecto t1 a)
             (triple-objecto t1 z)
             (graph-tripleo graph t1)]

       ;; recursion: there is a triple t2 that connects a to via and z is reachable from via
            [(triple-subjecto t2 a)
             (triple-objecto t2 via)
             (graph-tripleo graph t2)
             (reachableo graph (dec n) via z)]))

    l/fail))

(defn graph-collecto
  "Collect triples starting at a given subject."
  [graph n from t]
  (if (> n 0)

    (l/conde

     ;; base case: triples with subject
     [(triple-subjecto t from)
      (graph-tripleo graph t)]

     ;; recursion: collect triples with objects of from as subject
     [(fresh [t2 o]
             (triple-subjecto t2 from)
             (triple-objecto t2 o)
             (graph-tripleo graph t2)
             (graph-collecto graph (dec n) o t))])

    l/fail))

;; Description
 
(defn description-tripleo
  [desc p o]
  (graph-tripleo (rdf/description-graph desc)
                 (rdf/description-subject desc) p o))
