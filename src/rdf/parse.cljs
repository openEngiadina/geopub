(ns rdf.parse
  "Bindings to the rdf-parse.js library for parsing RDF from  B"
  (:require-macros [cljs.core.async :refer [go go-loop]])
  (:require [rdf.core :as rdf]
            [cljs.core.async :as async :refer [<! >!]]
            ["rdf-parse" :as rdf-parse]
            ["stream" :as stream]
            [rdf.js.data-model]))

;; The rdf-parse.js parser
(def parser (.-default rdf-parse))

;; Helpers to get data between streams and core.async
 
(defn put-stream!
  "Reads values from a readable stream and puts them on a channel. When the stream ends, the channel is closed."
  [ch stream]
  (.on stream "data" (fn [data]
                       (.pause stream)
                       (async/put! ch data #(.resume stream))))
  (.on stream "end" #(async/close! ch))
  ch)

(defn pipe-stream
  "Takes elements from the from channel and supplies them to the writable stream. When the channel is closed, the stream is ended. "
  [from to-stream]
  (go-loop []
    (let [data (<! from)]
      (if (some? data)
        (do (.write to-stream data)
            (recur))
        (.end to-stream data))))
  stream)


;; Parser

(defn content-types
  "Returns all content-types that can be parsed."
  []
  ;; This is copied from the output of parser.contentTypes(). See also https://github.com/rubensworks/rdf-parse.js#getting-all-known-content-types.
  '("application/n-quads"
    "application/trig"
    "application/n-triples"
    "text/turtle"
    "text/n3"
    "application/rdf+xml"
    "application/xml"
    "text/xml"
    "image/svg+xml"
    "text/html"
    "application/xhtml+xml"
    "application/ld+json"
    "application/json"))

(defn parse
  "Takes data from the input-channel and parses them to Triples. Returns a channel that contains triples. input-channel needs to be closed before all triples are emitted."
  [input-channel & {:keys [content-type base-iri]}]
  (let
      [input-stream (new (.-PassThrough stream))
       ;; cast to rdf/triple by transducing output channel
       output-channel (async/chan 1 (map rdf/triple))
       opts (clj->js {:contentType (or content-type "text/turtle")
                      :baseIRI (or base-iri "")})]

    ;; Put the stream from rdf-parse.js onto the output-channel
      (put-stream! output-channel
                   (.parse parser input-stream opts))

      ;; pipe the input-channel into the input-stream
      (pipe-stream input-channel input-stream)

      ;; Return the output-channel
      output-channel))

;; (defn print-channel
;;   "Prints anything that is put in the channel" []
;;   (let [c (async/chan)]
;;     (go-loop []
;;       (let [x (<! c)]
;;         (println x)
;;         (if x (recur))))
;;     c))


;; (def turtle-string "<a> <b> <c>, <d>, <e>.")

;; (go

;;   (let [printer (print-channel)
;;         input-channel (async/chan)]

;;     (async/pipe (parse input-channel) printer)

;;     (async/put! input-channel turtle-string)

;;     (async/close! input-channel)))
