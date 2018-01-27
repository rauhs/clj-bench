(defproject bench "0.1.0-SNAPSHOT"
  :dependencies [
                 ;[org.clojure/clojure "1.10.0-master-SNAPSHOT"]
                 [org.clojure/clojure "1.9.0"]
                 [criterium "0.4.4"]
                 [proteus "0.1.6"]]
  ;:main ^:skip-aot clj-bench.core
  :global-vars {*warn-on-reflection* true
                *assert* false}
  :target-path "target/%s"
  :jvm-opts ["-server"
             "-Xms4g"
             "-Xmx4g"
             "-Dclojure.spec.check-asserts=false"
             "-Dclojure.spec.compile-asserts=false"
             ;"-XX:+PrintCompilation"
             "-Dclojure.compiler.direct-linking=true"]
  :profiles {:uberjar {:aot :all}})
