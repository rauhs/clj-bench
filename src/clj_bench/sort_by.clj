(ns clj-bench.sort-by
  (:require
    [criterium.core :refer :all]))

(defn sort-by-
  ([keyfn coll]
   (sort-by- keyfn compare coll))
  ([keyfn ^java.util.Comparator comp coll]
   (if (seq coll)
     (let [ary (object-array (count coll))]
       (loop [i 0 xs (seq coll)]
         (when xs
           (let [x (first xs)]
             (aset ary i (object-array [(keyfn x) x]))
             (recur (inc i) (next xs)))))
       (. java.util.Arrays
          (sort ary (fn [x y] (. comp (compare (aget ^{:tag "[Ljava.lang.Object;"} x 0)
                                               (aget ^{:tag "[Ljava.lang.Object;"} y 0))))))
       (map (fn [x] (aget ^{:tag "[Ljava.lang.Object;"} x 1)) ary))
     ())))


(comment
  (def xs (repeatedly 500000 #(-> {:x (rand-int 1000)})))

  (with-progress-reporting
    (quick-bench (sort-by- :x xs) :verbose))

  (with-progress-reporting
    (quick-bench (sort-by :x xs) :verbose)))

