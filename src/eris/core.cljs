(ns eris.core
  (:require ["js-eris" :as js-eris]
            [rdf.core :as rdf]
            [cljs.core.async :as async :refer [>!]]
            [goog.crypt])
  (:require-macros [cljs.core.async :refer [go]]))


(defn- p->c [promise]
  "Wrap a JavaScript promise as a core.async channel."
  (let [c (async/chan)]
        (-> promise
            (.then (fn [res] (go (>! c res)
                                 (async/close! c))))
            (.catch (fn [err] (go (>! c err)
                                  (async/close! c)))))
        c))


(defn iri [data]
  "Returns the ERIS read capability for some data as rdf.core/iri in an async.core/channel"
  (async/map rdf/iri
             [(p->c (js-eris/put data))]))


(comment
  ;; Note that iri takes strings as well but as JavaScript uses UTF-16 this will
  ;; result in a different ERIS capability than in the documentation.
  ;; goog.crypt forces  encoding as UTF-8
  (go
    (println (<! (iri (goog.crypt/stringToUtf8ByteArray "Hail ERIS!"))))))
