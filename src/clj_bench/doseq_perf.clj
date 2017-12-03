(ns clj-bench.doseq-perf
  (:require
    [criterium.core :as crit :refer [quick-bench bench]])
  (:import (java.util.concurrent.atomic AtomicInteger)))

(set! *warn-on-reflection* true)


