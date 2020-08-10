(ns geopub.uri
  "Helpers for dealing with URIs (wrapper around goog.Uri)"
  (:require [goog.Uri]))

(defn parse [s]
  (goog.Uri/parse s))

(defn set-path [uri path]
  (if (uri? uri)
    (.setPath ^js uri path)
    (set-path (parse uri) path)))

(defn set-query [uri query]
  (if (uri? uri)
    (.setQuery ^js uri (->> query
                            (map (fn [[k v]] (str (name k) "=" v)))
                            (clojure.string/join "&")))
    (set-query (parse uri) query)))

(comment
  (set-query "http://example.com/" {:response 53}))
