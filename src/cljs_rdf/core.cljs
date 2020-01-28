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

(ns cljs-rdf.core
  "RDF in ClojureScript"
  (:require-macros [cljs-rdf.core :refer [defns]]
                   [cljs.core.logic :refer [run* fresh defne]])
  (:require [cljs.core.logic :as l]))

;; Commonly used namespaces

(defns rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(defns xsd "http://www.w3.org/2001/XMLSchema#")

;; Basic Interfaces

(defprotocol ITriple
  (triple-subject [x])
  (triple-predicate [x])
  (triple-object [x]))

(defn triple? [x]
  (satisfies? ITriple x))

(defprotocol IIRI
  (iri-value [x]))

(defn iri? [x]
  (satisfies? IIRI x))

(defprotocol IBlankNode
  (blank-node-id [x]))

(defn blank-node? [x]
  (satisfies? IBlankNode x))

(defprotocol ILiteral
  (literal-value [x])
  (literal-language [x])
  (literal-datatype [x]))

(defn literal? [x]
  (satisfies? ILiteral x))

;; Graph

(defprotocol IGraph
  (graph-add [x triple] "Add a triple to the dataset.")
  (graph-delete [x triple] "Remove triple from the dataset.")
  (graph-has [x triple] "Returns true if triple is in dataset, false if not."))

(defn graph? [x]
  (satisfies? IGraph x))

(defn graph-rel [seq q]
  "Unify graph into a relational program."
  (fn [s]
    (l/to-stream
     ;; TODO plenty of room for optimizing. Currently just treats graph as a seq.
     (map #(l/unify s % q) seq))))

;; ;; Implement protocol for basic types

;; (extend-type js/String
;;   INamedNode
;;   (named-node-iri [x] x)

;;   ILiteral
;;   (literal-value [x] (.valueOf x))
;;   (literal-language [x] nil)
;;   (literal-datatype [x] (xsd "string")))

;; (extend-type js/Number
;;   ILiteral
;;   (literal-value [x] (.valueOf x))
;;   (literal-language [x] nil)
;;   (literal-datatype [x]
;;     (do
;;       (cond
;;         (integer? (.valueOf x)) (xsd "integer")
;;         (double? (.valueOf x)) (xsd "double")))))


;; Implementation of data model using records

(declare ->Triple)

(defrecord Triple [subject predicate object]
  ITriple
  (triple-subject [q] (:subject q))
  (triple-predicate [q] (:predicate q))
  (triple-object [q] (:object q))

  l/IUnifyTerms
  (l/-unify-terms [u v s]
    (if (instance? Triple v)
      (-> s
          (l/unify (:subject u) (:subject v))
          (l/unify (:predicate u) (:predicate v))
          (l/unify (:object u) (:object v)))
      (l/unify s v u)))

  l/IWalkTerm
  (l/-walk-term [v s]
    (->Triple
     (l/-walk* s (:subject v))
     (l/-walk* s (:predicate v))
     (l/-walk* s (:object v))))

  l/IReifyTerm
  (l/-reify-term [v s]
    (-> s
        (l/-reify* (:subject v))
        (l/-reify* (:predicate v))
        (l/-reify* (:object v)))))

(defn triple [s p o]
  "Returns a triple."
  (->Triple s p o))

(defn triple-cast [v]
  "Cast something that implements ITriple into a triple."
  (triple
   (triple-subject v)
   (triple-predicate v)
   (triple-object v)))

;; (run* [q]
;;       (fresh [s p o]
;;              (l/== (triple s p o) (triple 1 2 3))
;;              (l/== (triple s p o) q)))

;; (run* [q]
;;   (fresh [s p o]
;;     (l/== (triple s p o) q)
;;     ))

(declare ->BlankNode)

(defrecord BlankNode [id]
  IBlankNode
  (blank-node-id [b] (:id b))

  l/IUnifyTerms
  (l/-unify-terms [u v s]
    (if (instance? BlankNode v)
      (l/unify s (:id u) (:id v))
      (l/unify s v u)))

  l/IWalkTerm
  (l/-walk-term [v s]
    (->BlankNode
     (l/-walk* s (:id v))))

  l/IReifyTerm
  (l/-reify-term [v s]
    (l/-reify* s (:id v))))

(defn blank-node
  "Returns a blank node. If no id is suplied a new (and unique) id will be generated."
  ([] (->BlankNode (gensym)))
  ([id] (->BlankNode id)))

(declare ->Literal)

(defrecord Literal [value language datatype]
  ILiteral
  (literal-value [l] (:value l))
  (literal-language [l] (:language l))
  (literal-datatype [l] (:datatype l))

  l/IUnifyTerms
  (-unify-terms [u v s]
    (if (instance? Literal v)
      (-> s
          (l/unify (:value u) (:value v))
          (l/unify (:language u) (:language v))
          (l/unify (:datatype u) (:datatype v)))
      (l/unify s v u)))

  l/IWalkTerm
  (l/-walk-term [v s]
    (->Literal
     (l/-walk* s (:value v))
     (l/-walk* s (:language v))
     (l/-walk* s (:datatype v))))

  l/IReifyTerm
  (l/-reify-term [v s]
    (-> s
        (l/-reify* (:value v))
        (l/-reify* (:language v))
        (l/-reify* (:datatype v)))))

(defn literal
  "Returns a literal with optional language and datatype."
  ([value & {:keys [language datatype]}]

   (cond

     ;; already a literal, nothing to do
     (instance? Literal value) value

     ;; satisfies the ILiteral protocol, cast to a Literal
     (literal? value)
     (->Literal (literal-value value)
                (or language (literal-language value))
                (or datatype (literal-datatype value)))

     ;; return a new Literal
     :else (->Literal value language datatype))))

;; (run* [q]
;;       (fresh [a]
;;              (l/membero a [1 2 3 4])
;;              (l/== (literal a) q)))

;; (run* [q]
;;       (fresh [a b c]
;;              (l/== (->Literal a b c) q)))

;; ;; Playground

;; (defrecord Something [v]

;;   l/IUnifyTerms
;;   (-unify-terms [u v s]
;;     (if (instance? Something v)
;;       (l/unify s (:v u) (:v v))
;;       (l/unify s v u)))

;;   l/IWalkTerm
;;   (l/-walk-term [v s]
;;     (->Something (l/-walk* s (:v v))))

;;   l/IReifyTerm
;;   (l/-reify-term [v s]
;;     (println "-reify-term " v s)
;;     (l/-reify* s (:v v))))

;; (satisfies? l/IUnifyTerms (->Something nil))

;; (run* [q]
;;       (fresh [a]
;;              (l/membero q [1 2 3 4])
;;              (l/== (->Something q) (->Something a))))

;; (run* [q]
;;       (fresh [a]
;;              (l/membero q [1 2 3 4])
;;              (l/== (->Something q) a)))

;; (run* [q]
;;       (fresh [a]
;;              (l/membero a [1 2 3])
;;              (l/== (->Something a) q)))

;; (run* [q]
;;       (l/== (->Something 1) 3))

;; (run* [q]
;;       (fresh [a b]
;;              (l/== b 2)
;;              (l/== a b)
;;              (l/== q (->Something a))))

;; (run* [q]
;;       (fresh [a]
;;              (l/== q (->Something a))))

