(ns rdf.ns
  "Common namespaces"
  (:require [rdf.core :as rdf])
  (:require-macros [rdf.core :refer [defns]]))


(defns rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(defns rdfs "http://www.w3.org/2000/01/rdf-schema#")
(defns owl "http://www.w3.org/2002/07/owl#")
(defns xsd "http://www.w3.org/2001/XMLSchema#")

;; example namespace (useful for testing)
(defns ex "http://example.com/")
