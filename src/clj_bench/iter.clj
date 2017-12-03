(ns clj-bench.iter
  (:require
    [clj-bench.loop-it :refer [loop-it multi-iter multi-iter-lazy]]
    [clj-bench.vec-reduce :refer [sv-loop]]
    [criterium.core :as crit :refer [quick-bench bench]]
    [proteus :refer :all])
  (:import (java.util Iterator)
           (java.util.concurrent.atomic AtomicInteger)
           (clojure.lang IndexedSeq)))

(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)

(def xs (vec (range 1e6)))
(def xs-large (vec (range 1e7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single reduce, primitve:

(defn sum-reduce [xs] (reduce + 0 xs))

(defn sum-loop-prim
  [xs]
  (loop [xs (seq xs), sum 0]
    (if xs
      (recur (next xs) (+ sum ^long (first xs)))
      sum)))

(defn sum-loop-iter
  [xs]
  (loop-it [x xs, :let [sum 0]]
    (recur (+ sum ^long x))
    sum))

(defn sum-loop
  [xs]
  (loop [xs (seq xs)
         sum (Long. 0)] ;; Avoid primitive math.
    (if xs
      (recur (next xs) (+ sum (first xs)))
      sum)))
#_(sum-loop xs-large)

(comment
  (quick-bench (sum-reduce xs-large)) ;; 177ms
  (quick-bench (sum-loop xs-large)) ;; 178ms
  (quick-bench (sum-loop-prim xs-large)) ;; 130ms
  (quick-bench (sum-loop-iter xs-large)) ;; 59ms
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ODD/EVEN accumulator:
(defn odd-even-reduce [xs]
  (reduce (fn [[odd even] x]
            (if (odd? x)
              [(inc odd) even]
              [odd (inc even)])) [0 0] xs))
(defn odd-even-reduce-count [xs]
  (reduce (fn [s x] (inc s)) 0 xs))
(defn odd-even-reduce-prim-array [xs]
  (reduce (fn [^longs arr x]
            (if (odd? x)
              (aset arr 0 (inc (aget arr 0)))
              (aset arr 0 (inc (aget arr 0))))
            arr)
          (long-array [0 0]) xs))
(defn odd-even-reduce-volatile [xs]
  (let [even (volatile! 0)
        odd (volatile! 0)]
    (reduce (fn [_ x]
              (if (odd? x)
                (vswap! even inc)
                (vswap! odd inc)))
            nil xs)
    [@odd @even]))
(defn odd-even-doseq [xs]
  (let [odd (volatile! 0)
        even (volatile! 0)]
    (doseq [x xs]
      (if (odd? x)
        (vswap! odd inc)
        (vswap! even inc)))
    [@odd @even]))
(defn odd-even-iter
  [xs]
  (let [it (clojure.lang.RT/iter xs)]
    (loop [odd 0
           even 0]
      (if (.hasNext it)
        (if (odd? (.next it))
          (recur (inc odd) even)
          (recur odd (inc even)))
        [odd even]))))
(defn odd-even-loop-it
  [xs]
  (loop-it [x xs
            :let [odd 0, even 0]]
    (if (odd? x)
      (recur (inc odd) even)
      (recur odd (inc even)))
    [odd even]))
#_(odd-even-loop-it xs-large)
(defn odd-even-mutable [xs]
  (let-mutable [odd 0
                even 0]
               (doseq [x xs]
                 (if (odd? x)
                   (set! odd (inc odd))
                   (set! even (inc even))))
               [odd even]))
(defn odd-even-doseq-atomic [xs]
  (let [odd (AtomicInteger. 0)
        even (AtomicInteger. 0)]
    (doseq [x xs]
      (if (odd? x)
        (.getAndIncrement odd)
        (.getAndIncrement even)))
    [(.get odd) (.get even)]))

(defn odd-even-doseq-array [xs]
  (let [odd (long-array [0])
        even (long-array [0])]
    (doseq [x xs]
      (if (odd? x)
        (aset odd 0 (inc (aget odd 0)))
        (aset even 0 (inc (aget even 0)))))
    [(aget odd 0) (aget even 0)]))
#_(odd-even-doseq-array xs-large)
(defn odd-even-loop
  [xs]
  (loop [xs (seq xs)
         odd 0
         even 0]
    (if xs
      (if (odd? (first xs))
        (recur (next xs) (inc odd) even)
        (recur (next xs) odd (inc even)))
      [odd even])))
(defn odd-even-chunked-loop
  [xs]
  (sv-loop [x xs
            :let [even 0, odd 0]]
    (if (odd? x)
      (recur even (inc odd))
      (recur (inc even) odd))
    [odd even]))
#_(odd-even-chunked-loop xs)

(comment
  (quick-bench (odd-even-reduce xs)) ;; 21.55ms
  (quick-bench (odd-even-reduce-prim-array xs)) ;; 9.78ms
  (quick-bench (odd-even-loop xs)) ;; 13.2ms
  (quick-bench (odd-even-iter xs)) ;; 6.65ms
  (quick-bench (odd-even-chunked-loop xs)) ;; 5.9ms
  (quick-bench (odd-even-doseq-atomic xs)) ;; 10.2ms
  (quick-bench (odd-even-mutable xs)) ;; 6.44ms
  )
(comment
  ;; 10 million:
  (quick-bench (odd-even-reduce xs-large)) ;; 222ms
  (quick-bench (odd-even-reduce-count xs-large)) ;; 82ms
  (quick-bench (odd-even-reduce-prim-array xs-large)) ;; 94ms
  (quick-bench (odd-even-loop xs-large)) ;; 139ms
  (quick-bench (odd-even-loop-boxed xs-large)) ;;
  (quick-bench (odd-even-iter xs-large)) ;; 70ms
  (quick-bench (odd-even-chunked-loop xs-large)) ;; 65ms
  (quick-bench (odd-even-doseq-atomic xs-large)) ;; 103ms
  (quick-bench (odd-even-doseq-array xs-large)) ;; 73ms
  (quick-bench (odd-even-mutable xs-large)) ;; 70ms
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; take-nth

(defn take-nth-vec
  "Eager! outputs a vector."
  [n xs]
  (let [n-1 (long (unchecked-dec n))]
    (loop-it [x xs
              :let [i 0, out (transient [])]]
      (if (zero? i)
        (recur n-1 (conj! out x))
        (recur (unchecked-dec i) out))
      (persistent! out))))

#_(= (take-nth-vec 2 xs) (take-nth 2 xs))
(comment
  (quick-bench (into [] (take-nth 5) xs-large)) ;; 254ms
  (quick-bench (doall (take-nth 5 xs-large))) ;; 262ms
  (quick-bench (take-nth-vec 5 xs-large))) ;; 52ms

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other core:
(defn interleave-fast
  [c1 c2]
  (loop-it [x1 c1, x2 c2
            :let [out (transient [])]]
    (recur (-> out (conj! x1) (conj! x2)))
    (persistent! out)))
(comment
  (quick-bench (doall (interleave xs xs))) ;; 64ms
  (quick-bench (interleave-fast xs xs)) ;; 26ms
  )
;; Also: take, take-while

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Partition-2

(defn partition-2-reduce
  [pred xs]
  (mapv persistent!
        (reduce (fn [[truthy falsy] x]
                  (if (pred x) [(conj! truthy x) falsy]
                               [truthy (conj! falsy x)]))
                [(conj!) (conj!)] xs)))

(defn partition-2
  [pred xs]
  (loop-it [x xs, :let [truthy (conj!), falsy (conj!)]]
    (if (pred x)
      (recur (conj! truthy x) falsy)
      (recur truthy (conj! falsy x)))
    [(persistent! truthy) (persistent! falsy)]))


(comment
  (quick-bench (partition-2-reduce even? xs-large)) ;; 291ms
  (quick-bench (partition-2 even? xs-large))) ;; 176ms

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterate multiple sequences:

(defn transpose
  [c1 c2]
  (loop-it [x1 c1, x2 c2
            :let [out (transient [])]]
    (recur (conj! out [x1 x2]))
    (persistent! out)))

(defn transpose-faster
  "Maybe returns a vector, maybe an IndexedSeq."
  [c1 c2]
  (if (and (counted? c1) (counted? c2))
    (let [out-count (min (count c1) (count c2))
          out (object-array out-count)]
      (loop-it [x1 c1, x2 c2
                :let [i 0]]
        (do
          (aset out i [x1 x2])
          (recur (inc i)))
        (seq out)))
    (loop-it [x1 c1, x2 c2
              :let [out (transient [])]]
      (recur (conj! out [x1 x2]))
      (persistent! out))))
(comment (quick-bench (mapv vector xs xs))
         (quick-bench (transpose xs xs))
         (quick-bench (transpose-faster xs xs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable number of collections

(defn mapv-fast-multi
  "NOTE: Can NOT deal with infinite sequences as arguments."
  [f & colls]
  (loop-it [xs (multi-iter colls)
            :let [out (conj!)]]
    (recur (conj! out (apply f xs)))
    (persistent! out)))

#_(= (mapv vector xs xs xs xs)
     (mapv-fast-multi vector xs xs xs xs))
(comment (quick-bench (mapv vector xs xs xs xs))
         (quick-bench (mapv-fast-multi vector xs xs xs xs)))

(defn mapv-fast-lazy
  "NOTE: Can deal with infinite sequences as arguments. Just like mapv in core"
  [f & colls]
  (loop-it [xs (multi-iter-lazy colls)
            :let [out (conj!)]]
    (recur (conj! out (apply f xs)))
    (persistent! out)))
#_(= (mapv vector xs xs xs xs)
     (mapv-fast-lazy vector xs xs xs xs))
(comment (quick-bench (mapv vector xs xs xs xs))
         (quick-bench (mapv-fast-lazy vector xs xs xs xs)))

;; And we combine it:

(defn mapv-fast
  "NOTE: Can deal with infinite sequences as arguments.
   Just like mapv in core"
  ([f coll]
   (loop-it [x coll, :let [out (conj!)]]
     (recur (conj! out (f x)))
     (persistent! out)))
  ([f c1 c2]
   (loop-it [x1 c1, x2 c2
             :let [out (conj!)]]
     (recur (conj! out (f x1 x2)))
     (persistent! out)))
  ([f c1 c2 c3]
   (loop-it [x1 c1, x2 c2, x3 c3
             :let [out (conj!)]]
     (recur (conj! out (f x1 x2 x3)))
     (persistent! out)))
  ([f c1 c2 c3 & colls]
   (loop-it [xs (if (counted? colls)
                  (multi-iter (list* c1 c2 c3 colls))
                  (multi-iter-lazy (list* c1 c2 c3 colls)))
             :let [out (conj!)]]
     (recur (conj! out (apply f xs)))
     (persistent! out)) ))


#_(apply mapv-fast #(-> %&) (repeat nil))
(comment (quick-bench (mapv vector xs xs xs xs)) ;; 1.28s
         (quick-bench (mapv-fast vector xs xs xs xs)) ;; 147ms
         )

(comment (quick-bench (mapv inc xs)) ;; 22ms
         (quick-bench (mapv-fast inc xs)) ;; 19ms
         )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mapcatv ;; like mapcat but not lazy!
(defn mapcatv
  "Mapcat. Eager and returns a vector. Slower if inner collection is small!"
  [f coll]
  (loop-it [xs coll, :let [out (transient [])]]
    (recur (loop-it [x (f xs), :let [out out]]
             (recur (conj! out x))
             out))
    (persistent! out)))

(defn mapcatv-2
  [f coll]
  (loop-it [xs coll, :let [out (transient [])]]
    (recur (reduce conj! out (f xs)))
    (persistent! out)))
#_(mapcatv vector (range 10))
#_(mapcatv-2 vector (range 10))
#_(mapcat vector (range 10))

(comment
  (quick-bench (into [] (mapcat vector) xs-large)) ;; 303ms
  (quick-bench (mapcatv vector xs-large)) ;; 527ms :(
  (quick-bench (mapcatv-2 vector xs-large))) ;; 263ms

(comment
  (quick-bench (into [] (mapcat #(vector % % % %)) xs)) ;; 73ms
  (quick-bench (mapcatv #(vector % % % %) xs)) ;; 89ms
  (quick-bench (mapcatv-2 #(vector % % % %) xs))) ;; 69ms


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduce single value

