(ns clj-bench.vec-reduce
  "Custom for PV's."
  (:require [clojure.walk :as walk])
  (:import
    (clojure.lang PersistentVector)))

(defn prefix-recur
  [form & prefixes]
  (walk/postwalk
    (fn [x]
      (if (seq? x)
        (let [[call & args] x]
          (if (= 'recur call)
            (list* 'recur (concat prefixes args))
            x))
        x))
    form))

(defmacro sv-loop
  "Single vector loop using chunkes similar to doseq.
   Note: Inflexible and not much faster than using iterators. So don't use this..."
  [[bind vec let-kw loop-bind] body finish]
  {:pre [(= let-kw :let) (vector? loop-bind)]}
  (assert (every? symbol? (take-nth 2 loop-bind)))
  (let [chunk- (with-meta (gensym "chunk_")
                          {:tag 'clojure.lang.IChunk})
        i- (gensym "i_")
        seq- (gensym "seq_")
        count- (gensym "count_")]
    `(let [~seq- (seq ~vec)]
       (assert (chunked-seq? ~seq-))
       (loop [~seq- ~seq-
              ~chunk- nil
              ~count- 0
              ~i- 0
              ;; Other loop bind
              ~@loop-bind]
         (if (< ~i- ~count-)
           (let [~bind (.nth ~chunk- ~i-)]
             ~(prefix-recur body seq- chunk- count- `(unchecked-inc ~i-)))
           (if (some? ~seq-)
             (let [c# (chunk-first ~seq-)]
               (recur (chunk-next ~seq-) c# (int (count c#)) 0
                      ;; TODO: Support destructing here?
                      ~@(take-nth 2 loop-bind)))
             ~finish))))))

