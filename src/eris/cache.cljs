(ns eris.cache
  "Content addressed caching with ERIS"
  (:require [rdf.core :as rdf]
            [rdf.fragment-graph :as fragment-graph]
            [rdf.ns :as ns]

            [eris.core :as eris]

            [cljs.core.async :as async :refer [<!]])
  (:require-macros [cljs.core.async :refer [go]]
                   [rdf.core :refer [defns]]))

(defns ec "http://purl.org/eris/cache/")
(defns prov "http://www.w3.org/ns/prov#")

(defn- create-prov-cache-activity [used generated]
  (-> (fragment-graph/fragment-graph "urn:dummy")
      (fragment-graph/add-statement (ns/rdf "type") (ec "CreateCache"))
      (fragment-graph/add-statement (prov "used")
                                    (fragment-graph/base-subject used))
      (fragment-graph/add-statement (prov "generated")
                                    (fragment-graph/base-subject generated))
      (eris/set-base-subject-to-eris-urn)))

(defn cache-fragment-graph-async [fg result]
  "Set base subject of Fragment Graph fg to ERIS read capability of fg and
    places on result channel together with the ERIS Cache Vocabulary activity."
  (go (let [eris-fg (<! (eris/set-base-subject-to-eris-urn fg))
            cache-activity (<! (create-prov-cache-activity fg eris-fg))]
        (async/put! result eris-fg)
        (async/put! result cache-activity)
        (async/close! result))))

