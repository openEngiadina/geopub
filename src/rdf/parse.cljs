(ns rdf.parse
  "Bindings to the rdf-parse.js library for parsing RDF from a bunch of different serializations."
  (:require-macros [cljs.core.async :refer [go go-loop]])
  (:require [rdf.core :as rdf]
            [cljs.core.async :as async :refer [<! >!]]
            ["rdf-parse" :as rdf-parse]
            ["stream" :as stream]
            ["setimmediate"]
            [rdf.js.data-model]))

;; The rdf-parse.js parser
(def parser (.-default rdf-parse))

;; Helpers to get data between streams and core.async
 
(defn put-stream!
  "Reads values from a readable stream and puts them on a channel. When the stream ends, the channel is closed. Errors will also be put on the channel."
  [ch stream]
  (.on stream "data" (fn [data]
                       (.pause stream)
                       (async/put! ch data #(.resume stream))))
  (.on stream "error" #(async/put! ch %))
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
    "application/ld+json"
    "application/json"
    "text/xml"
    "image/svg+xml"
    "text/html"
    "application/xhtml+xml"
    ))

(defn parse
  "Takes data from the input-channel and parses them to Triples. Returns a channel that contains triples. input-channel needs to be closed before all triples are emitted."
  [input-channel & {:keys [content-type base-iri]}]
  (let
      [input-stream (new (.-PassThrough stream))
       ;; cast to rdf/triple by transducing output channel, pass errors trough
       output-channel (async/chan 1 (map rdf/triple) identity)
       opts (clj->js {:contentType (or content-type "text/turtle")
                      :baseIRI (or base-iri "")})]

    ;; Put the stream from rdf-parse.js onto the output-channel
      (put-stream! output-channel
                   (.parse parser input-stream opts))

      ;; pipe the input-channel into the input-stream
      (pipe-stream input-channel input-stream)

      ;; Return the output-channel
      output-channel))


(defn parse-string
  "Parse string to triples. Returns a channel containing triples."
  [input & {:keys [content-type base-iri]}]
  (let [input-chan (async/chan)]
    (async/put! input-chan input #(async/close! input-chan))
    (parse input-chan
           :content-type content-type
           :base-iri base-iri)))
