(ns rdf.xsd.date-time
  (:require [rdf.core :as rdf]
            [rdf.ns :refer [xsd]]))

(defn now []
  (rdf/literal
   (.toISOString (new js/Date))
   :datatype (xsd "dateTime")))

