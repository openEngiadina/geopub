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

;; RDF/JS Data model specification (http://rdf.js.org/data-model-spec/)

;; TODO: implement equality

(defprotocol IQuad
  (quad-subject [x])
  (quad-predicate [x])
  (quad-object [x])
  (quad-graph [x]))

(defn quad? [x]
  (satisfies? IQuad x))

(defprotocol ITerm
  (term-type [x])
  (term-value [x]))

(defn term? [x]
  (satisfies? ITerm x))

(defprotocol INamedNode
  (named-node-iri [x]))

(defn named-node? [x]
  (satisfies? INamedNode x))

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

(defprotocol IVariable
  (variable-value [x]))

(defn variable? [x]
  (satisfies? IVariable x))

;; Dataset (inspired by RDF/JS Dataset specification, https://rdf.js.org/dataset-spec/)

(defprotocol IDataset
  (dataset-add [x quad] "Add a quad to the dataset.")
  (dataset-delete [x quad] "Remove quad from the dataset.")
  (dataset-has [x quad] "Returns true if quad is in dataset, false if not."))

(defn dataset? [x]
  (satisfies? IDataset x))

(defn dataset-rel [seq q]
  "Unify dataset into a relational program."
  (fn [s]
    (l/to-stream
     ;; TODO plenty of room for optimizing. Currently just treats dataset as a seq.
     (map #(l/unify s % q) seq))))

;; Implement protocol for basic types

(extend-type js/String
  INamedNode
  (named-node-iri [x] x)

  ILiteral
  (literal-value [x] (.valueOf x))
  (literal-language [x] nil)
  (literal-datatype [x] (xsd "string")))

(extend-type js/Number
  ILiteral
  (literal-value [x] (.valueOf x))
  (literal-language [x] nil)
  (literal-datatype [x]
    (do
      (cond
        (integer? (.valueOf x)) (xsd "integer")
        (double? (.valueOf x)) (xsd "double")))))


;; Implementation of data model using records

(declare ->Quad)

(defrecord Quad [subject predicate object graph]
  IQuad
  (quad-subject [q] (:subject q))
  (quad-predicate [q] (:predicate q))
  (quad-object [q] (:object q))
  (quad-graph [q] (:graph q))

  l/IUnifyTerms
  (l/-unify-terms [u v s]
    (if (instance? Quad v)
      (-> s
          (l/unify (:subject u) (:subject v))
          (l/unify (:predicate u) (:predicate v))
          (l/unify (:object u) (:object v))
          (l/unify (:graph u) (:graph v)))
      (l/unify s v u)))

  l/IWalkTerm
  (l/-walk-term [v s]
    (->Quad
     (l/-walk* s (:subject v))
     (l/-walk* s (:predicate v))
     (l/-walk* s (:object v))
     (l/-walk* s (:graph v))))

  l/IReifyTerm
  (l/-reify-term [v s]
    (-> s
        (l/-reify* (:subject v))
        (l/-reify* (:predicate v))
        (l/-reify* (:object v))
        (l/-reify* (:graph v)))))

(defn quad
  "Returns a Quad."
  ([q] (->Quad
        (quad-subject q)
        (quad-property q)
        (quad-object q)
        (quad-graph q)))
  ([s p o] (->Quad s p o nil))
  ([s p o g] (->Quad s p o g)))

;; (run* [q]
;;       (fresh [s p o]
;;              (l/== (quad s p o) (quad 1 2 3))
;;              (l/== (quad s p o) q)))

;; (run* [q]
;;   (fresh [s p o]
;;     (l/== (quad s p o) q)
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

