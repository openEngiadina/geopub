(ns cljs-rdf.turtle
  (:require [cljs-rdf.core :as rdf]))

(defn- base-directive [base]
  "")

(defn- prefix-directives [prefixes]
  "")

(defn- serialize-iri [iri]
  (str "<" (rdf/iri-value iri) ">"))

(defn- serialize-literal [literal]
  (str "\"" (rdf/literal-value literal) "\"" 

       (if (not-empty (rdf/literal-language literal))
         (str "@" (rdf/literal-language literal)))

       (if (rdf/literal-datatype literal)
         (str "^^" (serialize-iri (rdf/literal-datatype literal)))))
  )

(defn- serialize-object [obj]
  (cond
    (rdf/iri? obj)  (serialize-iri obj)
    (rdf/blank-node? obj) (str "_:" (rdf/blank-node-id obj))
    (rdf/literal? obj) (serialize-literal obj)))

(defn- triple-statement [triple]
  (let [s (rdf/triple-subject triple)
        p (rdf/triple-predicate triple)
        o (rdf/triple-object triple)]

    (str (serialize-object s) " "
         (serialize-object p) " "
         (serialize-object o) " .")))

(defn- statements [triples base prefix]
  (clojure.string/join "\n" (map triple-statement triples)))

(defn encode
  "Encode a sequence of triples as RDF Turtle"
  [triples & {:keys [base prefixes]}]
  (statements triples base prefixes))
