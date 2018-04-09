(ns clj-bench.map-chunking
  "See:
  https://dev.clojure.org/jira/browse/CLJ-2346"
  (:require [criterium.core :as crit]))

(defn doall-fast
  [s]
  (when-some [s (seq s)]
    (if (chunked-seq? s)
      (doall-fast (chunk-rest s))
      (doall-fast (next s)))))

(defn chunked-map [f coll]
  (when-some [s (seq coll)]
    (if (chunked-seq? s)
      (let [cf (chunk-first s)
            b (chunk-buffer (count cf))]
        (.reduce cf (fn [b x] (chunk-append b (f x)) b) b) ;; reduce instead of dotimes
        (chunk-cons (chunk b) (chunked-map f (chunk-rest s))))
      (cons (f (first s)) (map f (rest s))))))

(defn map-orig
  [f coll]
  (when-let [s (seq coll)]
    (if (chunked-seq? s)
      (let [c (chunk-first s)
            size (int (count c))
            b (chunk-buffer size)]
        (dotimes [i size]
          (chunk-append b (f (.nth c i))))
        (chunk-cons (chunk b) (map-orig f (chunk-rest s))))
      (cons (f (first s)) (map-orig f (rest s))))))

(let [xs (vec (range 4))]
  (crit/quick-bench (map-orig inc xs))
  (crit/quick-bench (chunked-map inc xs)))


