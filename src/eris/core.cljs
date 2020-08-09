(ns eris.core
  (:require ["js-eris" :as js-eris]

            [rdf.core :as rdf]
            [rdf.fragment-graph :as fragment-graph]
            [rdf.ns :as ns]

            [rdf.fragment-graph.csexp :as csexp]

            [cljs.core.async :as async :refer [>!]])
  (:require-macros [cljs.core.async :refer [go]]))


(defn- p->c [promise]
  "Wrap a JavaScript promise as a core.async channel."
  (let [c (async/promise-chan)]
        (-> promise
            (.then (fn [res] (go (>! c res)
                                 (async/close! c))))
            (.catch (fn [err] (go (>! c err)
                                  (async/close! c)))))
        c))


(defn iri [data]
  "Returns the ERIS read capability for some data as rdf.core/iri in an async.core/channel"
  (async/map rdf/iri [(p->c (js-eris/put data))]))

(defn set-base-subject-to-eris-urn [fg]
  "Set the base subject of Fragment Graph fg to the ERIS read capability of the
  canonical representation of the Fragment Graph."
  (async/map
   #(fragment-graph/set-base-subject fg %)
   [(iri (fragment-graph/->csexp fg))]))

;; (go
;;   (println
;;    (<!
;;     (set-base-subject-to-eris-urn
;;      (-> (fragment-graph/fragment-graph (ns/ex ""))
;;          (rdf/graph-add (rdf/triple (ns/ex "") (ns/ex "p") (rdf/literal 4 :datatype (ns/xsd "integer"))))
;;          (rdf/graph-add (rdf/triple (ns/ex "") (ns/ex "q") (ns/ex "#a")))
;;          (rdf/graph-add (rdf/triple (ns/ex "") (ns/ex "#q") (ns/ex "not-a-frag")))
;;          (rdf/graph-add (rdf/triple (ns/ex "#a") (ns/ex "r") (ns/ex "#b"))))))))
