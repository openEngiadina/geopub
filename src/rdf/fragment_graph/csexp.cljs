(ns rdf.fragment-graph.csexp
  "Canonical S-Expression"
  (:require [goog.crypt :as c]))

;; TODO move platform related stuff to platform.cljc/cljs
   
(defn string->byte-seq [s]
  (c/stringToUtf8ByteArray s))

(defn byte-seq->string [bs]
  (c/byteArrayToString bs))

(defn byte-seq-concat
  ([] (string->byte-seq ""))
  ([a & bs] (reduce (fn [acc x] (.concat acc x)) a bs)))

(comment
  (byte-seq-concat
   (string->byte-seq "hello ")
   (string->byte-seq "world")))

(defn- encode-netstring [sexp]
  (let [as-bytes (string->byte-seq sexp)]
    (byte-seq-concat
     (string->byte-seq (str (count as-bytes) ":"))
     as-bytes)))

(comment
  (byte-seq->string
   (encode-netstring "hello")))

(defn encode [sexp]
  "Encode S-Expression as Canonical S-Expression"
  (print sexp)
  (cond
    (string? sexp) (encode-netstring sexp)

    (symbol? sexp) (encode (name sexp))

    (keyword? sexp) (encode (name sexp))

    (number? sexp) (encode (str sexp))

    ;; a trick to prevent already encoded pieces to not be reencoded
    (:csexp sexp) (:csexp sexp)

    (list? sexp) (byte-seq-concat
                  (string->byte-seq "(")
                  (apply byte-seq-concat (map encode sexp))
                  (string->byte-seq ")"))))

(comment
  (byte-seq->string
   (encode '(rdf
             (s "http://example.com/" (l 42))))))

(comment
  (byte-seq->string
   (encode
    (list "not-yet-encoded"
          {:csexp (encode "already-encoded")}))))
