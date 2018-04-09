(ns clj-bench.spy-test
  (:require
    [spy.core :refer [spy]]))

(defn foo
  [x y]
  (spy [x y])
  (let [z (* x y)]
    (spy z)))
#_(spy [x y])
#_(apply foo (spy [x y]))

(defn bar [x]
  {:post [(spy bar %)]}
  (spy x)
  (* x x))

(comment
  (foo 1 2)
  (bar 22)

  )


























(comment
  ;; In CLJS:

  (defn fooo [x]
    (spy 'fooo x y))

  (defn bar [y]
    (spy 'bar x y))
  )




(comment

  (spy :all x)

  )

