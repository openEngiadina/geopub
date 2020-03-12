(ns rdf.graph.map.index
  "in-memory index"
  (:require [clojure.core.logic :as l]
            [clojure.set :as set]))

;; helpers
(defn- assoc-clean
  "If value is non-nil and non-empty replace value of key in map, else dissoc key."
  [map k v]
  (if (empty? v)
    (dissoc map k)
    (assoc map k v)
    ))

(defn- map-cons
  "cons a to every element of c"
  [c a]
  (map
   ;; cons a to element of c and make sure element of c is a seq.
   #(cons a (if (seq? %) % (list %)))
   c))

(defprotocol IIndex
  (index-merge [x y])
  (index-remove [x path])
  (index-match [x path]))

(extend-protocol IIndex
  nil
  (index-merge [_ _] nil)
  (index-remove [_ _] nil)
  (index-match [_ _] '())

  PersistentHashSet
  (index-merge [x y] (set/union x y))
  (index-remove [x path] (disj x (first path)))
  (index-match [index path]
    (let [k (first path)]
      (map list
           (if (l/lvar? k)
             (seq index)
             (filter (partial = k) index)))))

  PersistentHashMap
  (index-merge [x y] (merge-with index-merge x y))
  (index-remove [x path]
    (let [k (first path)
          rest (rest path)]
      (assoc-clean x k (index-remove (get x k) rest))))

  (index-match [index path]
    (let [k (first path)]
      
      (if (not (l/lvar? k))

        ;; k is concrete
        (->
         ;; get the sub-data we continue to match on
         (get index k)
         ;; match for the rest of the pattern
         (index-match (rest path))
         ;; add a to list of matches
         (map-cons k))

        ;; a is a logical variable. Iterate over all keys in the map and recursively match on concrete values.
        (mapcat #(index-match index (cons % (rest path)))
                (keys index))))))

